{-# LANGUAGE TemplateHaskell, QuasiQuotes, GADTs, DataKinds, FlexibleContexts, PatternSynonyms, PolyKinds #-}
module C where

import Data.Loc (noLoc)
import Data.Word

import Language.C.Quote.C
import qualified Language.C.Syntax as CSyntax

import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import NameMonad
import DSLD

fromTType :: TType -> CSyntax.Type
fromTType TFloat  = [cty|float|]
fromTType TWord32 = [cty|unsigned int|]
fromTType TBool   = [cty|int|]

-- We return (e, decls, stms), where e is the expression containing the result
-- of the computation (might be a variable), decls are the declarations
-- we need to emit in the beginning, and stms are the program statements.
tExpToC' :: TExp -> NameMonad (CSyntax.Exp, [CSyntax.InitGroup], [CSyntax.Stm])
tExpToC' (TVar v)                  = return ([cexp|$id:v|], [], [])
tExpToC' (LitI i)                  = return ([cexp|$uint:i|], [], [])
tExpToC' (Eq t1 t2)                =
  tExpToCOp2 (\e1 e2 -> [cexp| $exp:e1 == $exp:e2|]) t1 t2
tExpToC' (Or t1 t2)                =
  tExpToCOp2 (\e1 e2 -> [cexp| $exp:e1 || $exp:e2|]) t1 t2
tExpToC' (Let x ttype t1 t2)       = do
  (e1, i1, s1) <- tExpToC' t1
  (e2, i2, s2) <- tExpToC' t2
  return (e2,
          i1 ++ [[cdecl|$ty:(fromTType ttype) $id:x;|]] ++ i2,
          s1 ++ [cstms|$id:x = $exp:e1;|] ++ s2)
tExpToC' (TCnd ttype t1 t2 t3)     = do
  (e1, i1, s1) <- tExpToC' t1
  (e2, i2, s2) <- tExpToC' t2
  (e3, i3, s3) <- tExpToC' t3
  v <- newVar
  let v' = "v" ++ show v -- We introduce a variable to hold the result of the
                         -- if statement.
                         -- The other variables introduced by us start with 'x'.
  return ([cexp|$id:v'|],
          i1 ++ [[cdecl|$ty:(fromTType ttype) $id:(v');|]] ++ i2 ++ i3,
          s1 ++ [cstms|if ($exp:e1) { $stms:s2 $id:(v') = $exp:e2; }
                       else { $stms:s3 $id:(v') = $exp:e3; } |])
tExpToC' x                         = error $ show x

-- Code generation for binary operators
tExpToCOp2 op t1 t2 = do
  (e1, i1, s1) <- tExpToC' t1
  (e2, i2, s2) <- tExpToC' t2
  return (op e1 e2, i1 ++ i2, s1 ++ s2)

tExpToC :: TExp -> CSyntax.Func
tExpToC (Lam v1 targ tres t1) =
  let (e, decls, stms) = runNameMonad $ tExpToC' t1 in
  [cfun| $ty:(fromTType tres) f($ty:(fromTType targ) $id:v1)
            { $decls:decls $stms:stms return $exp:e; } |]

tExpToCBlock :: String -> TExp -> CSyntax.Stm
tExpToCBlock outvar (Lam v1 targ tres t1) =
  let (e, decls, stms) = runNameMonad $ tExpToC' t1 in
  [cstm| { $decls:decls $decl:parseCellDecl; $stms:(parseCellStms stms) $id:outvar = $exp:e; break; } |]
  where
  parseCellDecl = [cdecl|
    int x1 = parse_int(fieldz, &errmsg);
  |]
  parseCellStms stms = [cstms|
    if (errmsg != 0) {
      st->errmsg = errmsg;
      st->field = fieldz;
    } else {
      $stms:(exprInComments $ ppr t1)
      $stms:stms
    }
  |]
  -- This results in comment lines interspersed with empty
  -- statements.
  exprInComments doc = [[cstm|
    $comment:("// " ++ l);
  |] | l <- lines $ pretty 60 doc ]

-- Create a list of includes
includes :: [String] -> [CSyntax.Definition]
includes is = [ [cedecl| $esc:("#include <" ++ i ++ ">") |] | i <- is ]

cSVCallback1 :: CSyntax.Func
cSVCallback1 = [cfun|
  void cb1(void* field, typename size_t size, void* data) {
    ((int*) data)[0]++;
  } |]

cSVCallback2 :: CSyntax.Func
cSVCallback2 = [cfun|
  void cb2(int c, void* data) {
    ((int*) data)[0] = 0;
    ((int*) data)[1]++;
  } |]

-- for first row:
-- read column name
-- if column name is among the selected ones, save the index
-- otherwise ignore

-- We adhere to the convention that the free variable
-- of the property block is called "x1"
cSVCallbackProp1 :: [(Int, CSyntax.Stm)] -> CSyntax.Func
cSVCallbackProp1 fields = [cfun|
  void cbp1(void* field, typename size_t size, void* data) {
    struct statet* st = (struct statet*) data;
    char* fieldc = (char*) field;
    // Make a zero-terminated string.
    char* fieldz = alloc_field (field, size);
    if (st->j == 0 && st->errmsg == 0) {
      // We need this bogus statement to work around a problem with dangling else
      ;
      // First row contains column names
      for (int i = 0; i < st->len; ++i) {
        // We found a matching column name
        if (strcmp(st->col_names[i], fieldc) == 0) {
          if (st->col_inds[i] != -1) {
            // Error: we have seen this column name already
            st->errmsg = "Column appears twice";
          } else {
            st->col_inds[i] = st->i++;
          }
        }
      }
    // We want to short-cut on this condition, but libcsv might still feed more fields to us.
    } else if (st->errmsg == 0 && st->res != 0) {
      int fn = st->i++;
      char* errmsg = 0;
      int res = 1;
      int fn2 = -1;
      // This is very inefficient: we need to use a sorted array here
      // Or even an array of the size equal to the number of columns in the CSV file.
      for (int i = 0; i < st->len; ++i) {
        if(st->col_inds[i] == fn) {
          fn2 = i;
        }
      }
      switch(fn2) {
        $stms:cases
      }
      st->res = res;
      if (res == 0) {
        st->field = fieldz;
      } else {
        free(fieldz);
      }
    }
  } |]
  where cases = [[cstm| case $int:fn': $stm:ffun |] | (fn', ffun) <- fields] ++ [[cstm| case -1: break;|]]

-- Useful messages:
-- * mention the row, column and cell that fails the property
-- * mention the row, column and cell that fails parsing
--   - parsing can be the fst step of processing
--   - we probably need the Either type for that
-- 

-- We need to allocate a zero-terminated string
-- in order to use strtol(). It would be better to
-- avoid that for performance reasons.
parseAllocField :: CSyntax.Func
parseAllocField = [cfun|
  char* alloc_field(const char* str, int len) {
    char* buf = (char*) malloc(len+1);
    memcpy(buf, str, len);
    buf[len] = '\0';
    return buf;
  } |]

parseInt :: CSyntax.Func
parseInt = [cfun|
  int parse_int(const char* str, char** errmsg_out) {
    char *endptr;
    errno = 0;
    long l = strtol(str, &endptr, 0);
    int endptrnzero = *endptr != '\0';

    // On error buf will be deallocated by the caller.
    if (errno == ERANGE) {
      *errmsg_out = "Integer out of range";
      return 0;
    }
    if (endptrnzero || str == endptr) {
      *errmsg_out = "Invalid field: expected integer";
      return 0;
    }
    if (l < INT_MIN || l > INT_MAX) {
      *errmsg_out = "Integer out of range";
      return 0;
    }
  
    return (int) l;
  } |]

{-
-- parseInt can be implemented without having to copy memory
-- to yield a \0-terminated string
-- use unsigned types with wrapping to avoid overflow
parseInt2 :: CSyntax.Func
parseInt2 = [cfun|
  int parse_int (const char* str, int len, char** errmsg) {
    char *endptr;
    int res = 0;
    int cur = 0;
    int sign = 1;
    while (isspace(str[cur++]));
    if (str[cur] == '-') {
      sign = -1;
      cur++;
    }
    while (isdigit(str[cur])) {
      int newres = res + (str[cur] - '0') * sign;
      
    }
    long l = strtol(str, &endptr, 0);
    if (errno == ERANGE) {
      *errmsg = "Integer out of range";
      return 0;
    }
    if (*endptr != '\0' || str == endptr) {
      *errmsg = "Invalid field: expected integer";
      return 0;
    }
  
    if (l < INT_MIN || l > INT_MAX) {
      *errmsg = "Integer out of range";
      return 0;
    }
  
    return (int) l;
  } |]
-}

cSVCallbackProp2 :: CSyntax.Func
cSVCallbackProp2 = [cfun|
  void cbp2(int c, void* data) {
    struct statet* st = (struct statet*) data;
    // We want to short-cut on this condition, but libcsv might still feed more fields to us.
    if (st->j == 0 && st->errmsg == 0) {
      for (int i = 0; i < st->len; ++i) {
        if (st->col_inds[i] == -1) {
          st->errmsg = "Missing column";
          break;
        }
      }
      st->i = 0;
      st->j++;
    } else if (st->errmsg == 0 && st->res != 0) {
      st->i = 0;
      st->j++;
    }
  } |]

cSVCountState :: CSyntax.InitGroup
cSVCountState = [cdecl|
  int c[3] = {0, 0, 1}; |]

{-
 - res: 1 indicates that the property is still true
 - errmsg: 0 indicates that there was no parse error
 - prop_reason: reason why property failed
 - field: field relevant for errmsg or prop_reason (needs to be freed)
 - len: length of the col_names and col_inds arrays
 - col_names: array of selected column names
 - col_inds: mapping from the 'original' column index
 -           to the index in the actual csv file
 -           (TODO: turn it into sorted array)
 -}
cSVPropType :: CSyntax.Type
cSVPropType = [cty|struct statet { int i; int j; char* errmsg;
                                   char* prop_reason;
                                   char* field;
                                   int res; int len;
                                   char** col_names;
                                   int* col_inds; }|]

cSVPropStateInit :: [String] -> [CSyntax.BlockItem]
cSVPropStateInit l = [citems|
  struct statet c = {0, 0, 0, 0, 0, 1, $int:(length l), 0, 0};
  char* c_col_names[] = $init:(initListStrings l);
  c.col_names = c_col_names;
  int c_col_inds[$int:(length l)];
  c.col_inds = c_col_inds;
  for(int i = 0; i < c.len; ++i) {
    c.col_inds[i] = -1;
  }
|]

initListStrings :: [String] -> CSyntax.Initializer
initListStrings l = CSyntax.CompoundInitializer ([(Nothing, initString s) | s <- l ] ++ [(Nothing, [cinit| NULL|])]) noLoc
  where
  initString :: String -> CSyntax.Initializer
  initString s = [cinit| $string:s |]

--cSVPropStateAssign colNames = [cstms|
--  c.col_names = ]

-- We need to add short-cutting here too
readCSV :: CSyntax.Func
readCSV = [cfun|
  int read_csv(unsigned char sep, void* data,
               void (*cb1)(void *, typename size_t, void *),
               void (*cb2)(int, void *),
               typename FILE* x) {
    struct csv_parser p;
    char buf[1024];
    typename size_t bytes_read;

    csv_init(&p, CSV_STRICT);
    csv_set_delim(&p, sep);
    while ((bytes_read=fread(buf, 1, 1024, x)) > 0) {
      if (csv_parse(&p, buf, bytes_read, cb1, cb2, data) != bytes_read) {
        fprintf(stderr, "Error while parsing file: %s\n", csv_strerror(csv_error(&p)));
      }
    }
    csv_fini(&p, cb1, cb2, data);
    return 0;
  } |]

readCSVCount :: CSyntax.Func
readCSVCount = [cfun|
  int main() {
    typename FILE* x = fopen("test.csv", "r");
    $decl:cSVCountState ;
    read_csv('\t', &c,
                      cb1,
                      cb2,
                      x);
    printf("c[0] = %d, c[1] = %d\n", c[0], c[1]);
    return 0;
  } |]

-- Print a more helpful error message
-- How to interface with high-level?
-- Separate the printing of the message from rest of code?
-- Also: dynamic column numbers
-- We can have a stream of rows in the DSL?
readCSVProp :: [String] -> CSyntax.Func
readCSVProp l = [cfun|
  int main() {
    typename FILE* x = fopen("test.csv", "r");
    $items:(cSVPropStateInit l) ;
    read_csv('\t', &c,
                      cbp1,
                      cbp2,
                      x);
    if (c.res == 1 && c.errmsg == 0) {
      printf("{ \"result\": \"ok\" }\n");
    } else if (c.errmsg != 0) {
      printf("{\"result\": \"parse error\",\n\"row\": %d,\n\"column\": %d,\n\"reason\": \"%s\",\n\"field\": \"%s\" }\n", c.j, c.i - 1, c.errmsg, c.field);
      free(c.field);
    } else if (c.res != 1) {
      printf("{\"result\": \"failed\"\n\"row\": %d,\n\"column\": %d,\n\"reason\": \"Property failed\",\n\"field\": \"%s\" }\n", c.j, c.i - 1, c.field);
      free(c.field);
    }
    return 0;
  } |]

-- The generated code requires the libcsv library
-- https://github.com/rgamble/libcsv
mainReadCSV :: [CSyntax.Definition]
mainReadCSV = [cunit|
  $edecls:(includes ["csv.h"])
  $func:cSVCallback1
  $func:cSVCallback2
  $func:readCSV
  $func:readCSVCount
  |]


mainReadCSVProp :: [(Int, CSyntax.Stm)] -> [String] -> [CSyntax.Definition]
mainReadCSVProp l l2 = [cunit|
  $edecls:(includes ["string.h", "errno.h", "limits.h", "csv.h"])
  $ty:cSVPropType;
  $func:parseAllocField
  $func:parseInt
  $func:(cSVCallbackProp1 l)
  $func:cSVCallbackProp2
  $func:readCSV
  $func:(readCSVProp l2)
  |]


