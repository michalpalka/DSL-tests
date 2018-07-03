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



