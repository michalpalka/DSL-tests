{-# LANGUAGE TemplateHaskell, QuasiQuotes, GADTs, DataKinds, FlexibleContexts, PatternSynonyms, PolyKinds #-}
module Test4 where

import Data.List (find)
import Data.Word
import Control.Monad.State

import Language.Haskell.TH.Syntax (Q)
import qualified Language.Haskell.TH.Syntax (TExp)

import QHaskell hiding (get, (<+>))
import QHaskell.Expression.Utils.Show.GADTFirstOrder ()
import qualified QHaskell.Singleton as S
import QHaskell.Type.GADT (Typ(Wrd, Flt, Bol))

import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Language.C.Quote.C
import qualified Language.C.Syntax as CSyntax

plus :: Float -> Float -> Float
plus = (+)

mul :: Float -> Float -> Float
mul = (*)

(***) :: Float -> Float -> Float
a *** b = a * b



-- concrete sytntax + abstract syntax + mapping (part of parsing)
-- physical field + logical field + mapping
-- Income = NoData | IData Float
-- IncomePhy = -9 | -8 | -7 | 0 | *
--  -9, -8, -7, 0 -> NoData; x -> IData x

-- Education = NoData | HigherEd | OtherEd | NoEd
-- EducationPhy = -9 | -8 | -6 | 1 | 2 | 3
-- -9, -8, -6 -> NoData; 1 -> HigherEd; 2 -> OtherEd; 3 -> NoEd

-- the smoking data file has 1500 attributes!

data FieldT = FSum [Int] AtomT
  deriving (Eq, Show)

data AtomT = FInt | FFloat | FNone
  deriving (Eq, Show)

data LFieldT = LFT [(String, AtomT)]
  deriving (Eq, Show)

data Mapping = Mapping [([Word32], String)] (Maybe String)
  deriving (Eq, Show)

class WellFormed a where
  wellFormed :: a -> Bool    -- not used yet (but should check if the concrete syntax is "ok" = can be mapped to some abstract syntax)

incomePhy = FSum [-9, -8, -7, 0] FFloat

income = LFT [("NoData", FNone), ("IData", FFloat)]

incomeMap = Mapping [([-9, -8, -7, 0], "NoData")] (Just "IData")

data Atom = FVInt Word32 | FVFloat Float | FVNone
  deriving (Eq, Show)

data LField = LF String Atom
  deriving (Eq, Show)

--type MyMaybe a = (a -> b) -> b -> b

type MapFun = Atom -> Maybe LField

evalMapping :: Mapping -> MapFun
evalMapping (Mapping l m) (FVInt i) =
  case find (\(sp, _) -> i `elem` sp) l of
    Just (_, lab) -> Just $ LF lab FVNone
    Nothing       ->
      case m of
        -- We have a catch-all clause
        Just lab -> Just $ LF lab (FVInt i)
        Nothing  -> Nothing -- Invalid data value
evalMapping (Mapping _ m) (FVFloat f) =
  case m of
    -- We have a catch-all clause
    Just lab -> Just $ LF lab (FVFloat f)
    Nothing  -> Nothing -- Invalid data value

-- Reimplement evalMapping using low-level conditionals
--



(|->) :: [Word32] -> String -> ([Word32], String)
patterns |-> result = (patterns, result)

defCase :: [([Word32], String)] -> String -> Mapping
defCase l d = Mapping l (Just d)

noDefCase :: [([Word32], String)] -> () -> Mapping
noDefCase l d = Mapping l Nothing

incomeMapQ :: Mapping
incomeMapQ =
  [[-9, -8, -7, 0] |-> "NoData"]
  `defCase` "IData"

eduMapQ :: Mapping
eduMapQ =
  [[9, 8, 6]    |-> "NoData",
   [1]          |-> "HigherEd",
   [2]          |-> "OtherEd",
   [3]          |-> "NoEd"]
  `noDefCase` ()

--while :: HasSin Typ s =>
--  (Rep s) -> (s -> Bool) -> (s -> s) -> s -> s

-- \x -> case evalMapping (eduMapQ x) of
--          Just _  -> 1
--          Nothing -> 0

intEq :: Word32 -> Word32 -> Bool
intEq = (==)

makeQDSL "TestLang" ['plus, 'mul, 'intEq, '(||)]


pattern PlusVar   m n   = Prm Zro (Ext m (Ext n Emp))
pattern MulVar    m n   = Prm (Suc Zro) (Ext m (Ext n Emp))
pattern IEqVar    m n   = Prm (Suc (Suc Zro)) (Ext m (Ext n Emp))
pattern IOrVar    m n   = Prm (Suc (Suc (Suc Zro))) (Ext m (Ext n Emp))

testLang :: Qt Float -> ErrM Float
testLang q = do d <- translate q
                return (compile (normalise True d))


compile :: Type a => TestLang a -> a
compile = evaluate

myTest :: Qt (Float -> Float)
myTest = [|| \a -> mul (plus 2 1) a ||]


myApply :: Qt (Float -> Float) -> Qt Float -> Qt Float
myApply f x = [|| $$f $$x ||]

data TType = TFloat | TWord32 | TBool
  deriving (Eq, Show)

fromTType :: TType -> CSyntax.Type
fromTType TFloat  = [cty|float|]
fromTType TWord32 = [cty|unsigned int|]
fromTType TBool   = [cty|int|]

data TExp =
    Lit   Float
  | LitI  Word32
  | VarB  String
  | Fix   String TExp
  | Let   String TType TExp TExp
  | TVar  String
  | Lam   String TType TType TExp
  | TCnd  TType TExp TExp TExp
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

instance Pretty TExp where
  pprPrec n (Lit l)         = float l
  pprPrec n (LitI l)        = int $ fromIntegral l
  pprPrec n (TVar l)        = text l
  pprPrec n (Lam x _ _ t)       = parensIf (n > 0) $ nest 2 $
    text "\\" <> text x <+> text "->" <+> pprPrec 0 t
  pprPrec n (Let x _ t1 t2)   =
    text "let" <+> text x <+> equals <+> pprPrec 0 t1 <> line <>
      text "in" <+> pprPrec 0 t2
  pprPrec n (TCnd _ t1 t2 t3) = group $
    text "if" <+> pprPrec 0 t1 <+/>
      text "then" <+> nest 2 (pprPrec 0 t2) <+/>
      text "else" <+> nest 2 (pprPrec 0 t3)
  pprPrec n (Eq t1 t2)      = parensIf (n > 4) $
    pprPrec 4 t1 <+> text "==" <+> pprPrec 4 t2
  pprPrec n (Or t1 t2)      = parensIf (n > 3) $
    pprPrec 3 t1 <+> text "||" <+> pprPrec 3 t2
  --pprPrec n (And t1 t2)   = parensIf (n > 2) $
  --  pprPrec 2 t1 <+> text "&&" <+> pprPrec 2 t2

type NameMonad a = State Word32 a

newVar :: NameMonad Word32
newVar = do n <- get
            let n' = n + 1
            put n'
            return n'

-- | runs the given name monad
runNameMonad :: NameMonad a -> a
runNameMonad = flip evalState 0

-- We need this to remember the
-- type from the let binding
typeRep :: Type a => TestLang a -> TType
typeRep t = case aux t of
    Wrd -> TWord32
    Flt -> TFloat
    Bol -> TBool
  where
  aux :: Type a => TestLang a -> Typ a
  aux _ = S.sin

argTypeRep :: Type a => TestLang (a -> b) -> TType
argTypeRep t = case aux t of
    Wrd -> TWord32
    Flt -> TFloat
  where
  aux :: Type a => TestLang (a -> b) -> Typ a
  aux _ = S.sin

toBackEnd :: TestLang a -> NameMonad TExp
toBackEnd l = case l of
  ConF     i     -> pure (Lit  i)
  ConI     i     -> pure (LitI i)
  PlusVar  m n   -> toBackEnd2 Plus m n
  MulVar   m n   -> toBackEnd2 Mul m n
  IEqVar   m n   -> toBackEnd2 Eq m n
  IOrVar   m n   -> toBackEnd2 Or m n
  Som      m     -> fmap Som2 $ toBackEnd m
  Non            -> pure Non2
  May      m n p -> toBackEnd3 May2 m n p
  Int      x     -> return $ TVar $ "x" ++ show x
  LeT      e b   -> do
    x <- newVar
    e' <- toBackEnd e
    b' <- toBackEnd $ substitute (Int x) b
    return $ Let ("x" ++ show x) (typeRep e) e' b'
  Cnd      c t e -> do
    c' <- toBackEnd c
    t' <- toBackEnd t
    e' <- toBackEnd e
    return $ TCnd (typeRep t) c' t' e'
  a@(Abs      m) -> do
    x <- newVar
    let m2 = substitute (Int x) m
    m' <- toBackEnd m2
    return $ Lam ("x" ++ show x) (argTypeRep a) (typeRep m2) m'
  x -> error (show x)
  where
  toBackEnd2 :: (TExp -> TExp -> TExp) ->
                 TestLang a -> TestLang b -> NameMonad TExp
  toBackEnd2 c m n = c <$> toBackEnd m <*> toBackEnd n
  toBackEnd3 :: (TExp -> TExp -> TExp -> TExp) ->
                 TestLang a -> TestLang b -> TestLang c -> NameMonad TExp
  toBackEnd3 c m n p = c <$> toBackEnd m <*> toBackEnd n <*> toBackEnd p

toTExp :: Mapping -> (Maybe String -> Q (Language.Haskell.TH.Syntax.TExp Word32)) -> TExp
toTExp (Mapping l def) f =
  runExample [|| \x -> $$(mainBody l Nothing) x ||]
  where
  mainBody []               Nothing = [|| \x -> $$(f Nothing) ||]
  mainBody ((vals, lab):xs) md      = [|| \x -> if $$(conds vals) x then $$(f $ Just lab) else $$(mainBody xs md) x ||]
    where
      conds [c]    = [|| \x -> x `intEq` c ||]
      conds (c:cs) = [|| \x -> (x `intEq` c) || ($$(conds cs) x) ||]

-- We return (e, decls, stms), where e is the expression containing the result
-- of the computation (might be a variable), decls are the declarations
-- we need to emit in the beginning, and stms are the program statements.
tExpToC' :: TExp -> NameMonad (CSyntax.Exp, [CSyntax.InitGroup], [CSyntax.Stm])
tExpToC' (TVar v)                  = return ([cexp|$id:v|], [], [])
tExpToC' (LitI i)                  = return ([cexp|$uint:i|], [], [])
tExpToC' (Eq t1 t2)                = do
  (e1, i1, s1) <- tExpToC' t1
  (e2, i2, s2) <- tExpToC' t2
  return ([cexp| $exp:e1 == $exp:e2|], i1 ++ i2, s1 ++ s2)
tExpToC' (Or t1 t2)                = do
  (e1, i1, s1) <- tExpToC' t1
  (e2, i2, s2) <- tExpToC' t2
  return ([cexp| $exp:e1 || $exp:e2|], i1 ++ i2, s1 ++ s2)
tExpToC' (Let x ttype t1 t2)       = do
  (e1, i1, s1) <- tExpToC' t1
  (e2, i2, s2) <- tExpToC' t2
  return (e2, i1 ++ [[cdecl|$ty:(fromTType ttype) $id:x;|]] ++ i2, s1 ++ [cstms|$id:x = $exp:e1;|] ++ s2)
tExpToC' (TCnd ttype t1 t2 t3)     = do
  (e1, i1, s1) <- tExpToC' t1
  (e2, i2, s2) <- tExpToC' t2
  (e3, i3, s3) <- tExpToC' t3
  v <- newVar
  let v' = "v" ++ show v -- The other variables introduced by us start with 'x'
  return ([cexp|$id:v'|], i1 ++ [[cdecl|$ty:(fromTType ttype) $id:(v');|]] ++ i2 ++ i3, s1 ++ [cstms|if ($exp:e1) { $stms:s2  $id:(v') = $exp:e2; } else { $stms:s3  $id:(v') = $exp:e3; } |])
tExpToC' x                         = error $ show x

tExpToC :: TExp -> CSyntax.Func
tExpToC (Lam v1 targ tres t1) =
  let (e, decls, stms) = runNameMonad $ tExpToC' t1 in
  [cfun| $ty:(fromTType tres) f($ty:(fromTType targ) $id:v1) { $decls:decls $stms:stms return $exp:e; } |]


prop1 :: Maybe String -> Q (Language.Haskell.TH.Syntax.TExp Word32)
prop1 = propify prop1'

propify :: (Maybe String -> Bool) -> Maybe String -> Q (Language.Haskell.TH.Syntax.TExp Word32)
propify f x = [|| $$(g $ f x) ||]
  where
  g True  = [|| 1 ||]
  g False = [|| 0 ||]

prop1' :: Maybe String -> Bool
prop1' (Just "NoData")   = False
prop1' (Just "HigherEd") = True
prop1' (Just "OtherEd")  = True
prop1' (Just "NoEd")     = True
prop1' Nothing           = False


-- if (x == -9 || x == -8 || x == -6) {
--   return 0;
-- } if (x == 1) {
--   return 1;
-- } if (x == 2) {
--   return 1
-- } if (x -- 3) {
--   return 1;
-- } else {
--   return 0;
-- }

test1 :: Qt Float
test1 = myApply myTest [|| 7 ||]

-- The type signatures require more imports
--myNorm
--  :: QHaskell.Singleton.HasSin QHaskell.Type.GADT.Typ a =>
--     Qt a -> TestLang a
myNorm ex =
  case fmap (normalise True) $ translate ex
  of Rgt e -> e; e' -> error (show e')

--runExample
--  :: QHaskell.Singleton.HasSin QHaskell.Type.GADT.Typ a =>
--     Qt a -> TExp
runExample ex =
  runNameMonad $ toBackEnd $ myNorm ex

-- run: runExample test1

myMaybe = [|| \d f m -> case m of Nothing -> d; Just x -> f x ||]

hello = [|| 7 *** 4 :: Float ||]

test2 = [|| \x -> x `plus` 3 ||]
test3 = [|| \x -> if (x :: Word32) `intEq` 0 then (1 :: Word32) else 2 ||]

test4 = [|| \x -> $$myMaybe 5 (\y -> y) (if (x :: Word32) `intEq` 0 then Just (1 :: Word32) else Nothing) ||]

test5 = [|| case (Just (1 :: Word32)) of Nothing -> (5::Word32); Just x -> x ||]
test6 = [|| Just (1 :: Word32) ||]

-- To pretty-print
-- putDocLn $ ppr test7
-- To generate C code
-- putDocLn $ ppr $ tExpToC test7

test7 = toTExp eduMapQ prop1


