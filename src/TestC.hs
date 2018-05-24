{-# LANGUAGE QuasiQuotes #-}
module TestC where

import Language.C.Quote.C
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Data.Word

f1 n = [cunit|int add(int x) { return x + $int:n; }|]

pretty :: Pretty a => a -> IO ()
pretty = putDoc . ppr

f2 = [cexp|3.0 + 6.0|]

data TExp =
    Lit   Float
  | LitI  Word32
  | VarB  String
  | Fix   String TExp
  | Let   String TExp TExp
  | TVar  String
  | Lam   String TExp
  | TCnd  TExp TExp TExp
  | Fst   TExp
  | Snd   TExp
  | Som2  TExp
  | Non2
  | May2  TExp TExp TExp
  | Pair  TExp TExp
  | Plus  TExp TExp
  | Mul   TExp TExp
  | Eq    TExp TExp
  | Or    TExp TExp
  deriving (Eq, Show)

toCexp (Lit x) = [cexp|$float:x|]
toCexp (LitI n) = [cexp|$int:n|]
toCexp (TVar a) = [cexp|$id:a|]
toCexp (Plus a b) = [cexp|$exp:e1 + $exp:e2|]
  where e1 = toCexp a
        e2 = toCexp b
toCexp (Mul a b) = [cexp|$exp:e1 * $exp:e2|]
  where e1 = toCexp a
        e2 = toCexp b
toCexp (Eq a b) = [cexp|$exp:e1 == $exp:e2|]
  where e1 = toCexp a
        e2 = toCexp b
toCexp (Or a b) = [cexp|$exp:e1 || $exp:e2|]
  where e1 = toCexp a
        e2 = toCexp b

toC (Let x e b) = undefined
toC (Lam x e) = undefined


