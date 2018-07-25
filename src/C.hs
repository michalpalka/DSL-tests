{-# LANGUAGE TemplateHaskell, QuasiQuotes, GADTs, DataKinds, FlexibleContexts, PatternSynonyms, PolyKinds #-}
module C where

import Data.Word

import Language.C.Quote.C
import qualified Language.C.Syntax as CSyntax

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

tExpToCBlock :: TExp -> CSyntax.Stm
tExpToCBlock (Lam v1 targ tres t1) =
  let (e, decls, stms) = runNameMonad $ tExpToC' t1 in
  [cstm| { $decls:decls $stms:stms return $exp:e; } |]

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

-- We adhere to the convention that the free variable
-- of the property block is called "x1"
cSVCallbackProp1 :: [(Int, CSyntax.Stm)] -> CSyntax.Func
cSVCallbackProp1 fields = [cfun|
  void cb1(void* field, typename size_t size, void* data) {
    struct statet* st = (struct statet*) data;
    if (st->errmsg == 0) {
      int fn = st->i++;
      char* errmsg = 0;
      int x1 = parse_int((char*)field, &errmsg);
      if(errmsg != 0) {
        st->errmsg = errmsg;
      }
      switch(fn) {
      // FIXME: Need to take result from here and do something with it
        $stms:cases
      }
    }
  } |]
  where cases = [[cstm| case $int:fn': $stm:ffun |] | (fn', ffun) <- fields]

parseInt :: CSyntax.Func
parseInt = [cfun|
  int parse_int(const char* str, char** errmsg) {
    char *endptr;
    errno = 0;
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

cSVCallbackProp2 :: CSyntax.Func
cSVCallbackProp2 = [cfun|
  void cb2(int c, void* data) {
    struct statet* st = (struct statet*) data;
    st->i= 0;
    st->j++;
  } |]

cSVCountState :: CSyntax.InitGroup
cSVCountState = [cdecl|
  int c[3] = {0, 0, 1}; |]

cSVProcessType :: CSyntax.Type
cSVProcessType = [cty|struct statet { int i; int j; char* errmsg; }|]

cSVProcessState :: CSyntax.InitGroup
cSVProcessState = [cdecl|
  struct statet c = {0, 0, 0}; |]

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

readCSVProcess :: CSyntax.Func
readCSVProcess = [cfun|
  int main() {
    typename FILE* x = fopen("test.csv", "r");
    $decl:cSVProcessState ;
    read_csv('\t', &c,
                      cb1,
                      cb2,
                      x);
    printf("c.i = %d, c.j = %d, c.errmsg = %s\n", c.i, c.j, c.errmsg);
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


mainReadCSVProp :: [(Int, CSyntax.Stm)] -> [CSyntax.Definition]
mainReadCSVProp l = [cunit|
  $edecls:(includes ["errno.h", "limits.h", "csv.h"])
  $ty:cSVProcessType;
  $func:parseInt
  $func:(cSVCallbackProp1 l)
  $func:cSVCallbackProp2
  $func:readCSV
  $func:readCSVProcess
  |]


