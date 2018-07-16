{-# LANGUAGE TemplateHaskell, QuasiQuotes, GADTs, DataKinds, FlexibleContexts, PatternSynonyms, PolyKinds #-}
module DSLD where

import Data.List (find)
import Data.Word
import Data.Monoid -- ((<>))

import Language.Haskell.TH.Syntax (Q)
import qualified Language.Haskell.TH.Syntax (TExp)

import QHaskell hiding (get, (<+>))
import QHaskell.Expression.Utils.Show.GADTFirstOrder ()
import qualified QHaskell.Singleton as S
import QHaskell.Type.GADT (Typ(Wrd, Flt, Bol))

import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import NameMonad

import Language.C.Quote.C


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
-- -9, -8, -7, 0 -> NoData; x -> IData x

-- Education = NoData | HigherEd | OtherEd | NoEd
-- EducationPhy = -9 | -8 | -6 | 1 | 2 | 3
-- -9, -8, -6 -> NoData; 1 -> HigherEd; 2 -> OtherEd; 3 -> NoEd

-- the smoking data file has 1500 attributes!

-- Type of physical fields (not used for anything ATM)
data FieldT = FSum [Int] AtomT
  deriving (Eq, Show)

data AtomT = FInt | FFloat | FNone
  deriving (Eq, Show)

-- Type of logical fields (not used for anything ATM)
data LFieldT = LFT [(String, AtomT)]
  deriving (Eq, Show)

-- Mapping between a physical field and a logical field
data Mapping = Mapping [([Word32], String)] (Maybe String)
  deriving (Eq, Show)

-- not used yet (but should check if the concrete syntax
-- is "ok" (can be mapped to some abstract syntax)
class WellFormed a where
  wellFormed :: a -> Bool

incomePhy :: FieldT
incomePhy = FSum [-9, -8, -7, 0] FFloat

income :: LFieldT
income = LFT [("NoData", FNone), ("IData", FFloat)]

incomeMap :: Mapping
incomeMap = Mapping [([-9, -8, -7, 0], "NoData")] (Just "IData")

data Atom = FVInt Word32 | FVFloat Float | FVNone
  deriving (Eq, Show)

data LField = LF String Atom
  deriving (Eq, Show)

type MapFun = Atom -> Maybe LField

-- Not used for code generations
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

intEq :: Word32 -> Word32 -> Bool
intEq = (==)

makeQDSL "DSLD" ['plus, 'mul, 'intEq, '(||)]

pattern PlusVar   m n   = Prm Zro (Ext m (Ext n Emp))
pattern MulVar    m n   = Prm (Suc Zro) (Ext m (Ext n Emp))
pattern IEqVar    m n   = Prm (Suc (Suc Zro)) (Ext m (Ext n Emp))
pattern IOrVar    m n   = Prm (Suc (Suc (Suc Zro))) (Ext m (Ext n Emp))

dsld :: Qt Float -> ErrM Float
dsld q = do d <- translate q
            return (compile (normalise True d))

compile :: Type a => DSLD a -> a
compile = evaluate

-- Type witnesses for TExp
data TType = TFloat | TWord32 | TBool
  deriving (Eq, Show)

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

-- We need this to remember the
-- type from the let binding
typeRep :: Type a => DSLD a -> TType
typeRep t = case aux t of
    Wrd -> TWord32
    Flt -> TFloat
    Bol -> TBool
  where
  aux :: Type a => DSLD a -> Typ a
  aux _ = S.sin

argTypeRep :: Type a => DSLD (a -> b) -> TType
argTypeRep t = case aux t of
    Wrd -> TWord32
    Flt -> TFloat
  where
  aux :: Type a => DSLD (a -> b) -> Typ a
  aux _ = S.sin

toBackEnd :: DSLD a -> NameMonad TExp
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
                 DSLD a -> DSLD b -> NameMonad TExp
  toBackEnd2 c m n = c <$> toBackEnd m <*> toBackEnd n
  toBackEnd3 :: (TExp -> TExp -> TExp -> TExp) ->
                 DSLD a -> DSLD b -> DSLD c -> NameMonad TExp
  toBackEnd3 c m n p = c <$> toBackEnd m <*> toBackEnd n <*> toBackEnd p

toTExp :: Mapping -> (Maybe String -> Bool) -> TExp
toTExp (Mapping l def) f =
  runExample [|| \x -> $$(mainBody l Nothing) x ||]
  where
  f' = propify f
  mainBody []               Nothing = [|| \x -> $$(f' Nothing) ||]
  mainBody ((vals, lab):xs) md      = [|| \x -> if $$(conds vals) x then $$(f' $ Just lab) else $$(mainBody xs md) x ||]
    where
      conds [c]    = [|| \x -> x `intEq` c ||]
      conds (c:cs) = [|| \x -> (x `intEq` c) || ($$(conds cs) x) ||]

propify :: (Maybe String -> Bool) -> Maybe String -> Q (Language.Haskell.TH.Syntax.TExp Word32)
propify f x = [|| $$(g $ f x) ||]
  where
  g True  = [|| 1 ||]
  g False = [|| 0 ||]

-- The type signatures require more imports
--myNorm
--  :: QHaskell.Singleton.HasSin QHaskell.Type.GADT.Typ a =>
--     Qt a -> DSLD a
myNorm ex =
  case fmap (normalise False) $ translate ex
  of Rgt e -> e; e' -> error (show e')

--runExample
--  :: QHaskell.Singleton.HasSin QHaskell.Type.GADT.Typ a =>
--     Qt a -> TExp
runExample ex =
  runNameMonad $ toBackEnd $ myNorm ex

myMaybe = [|| \d f m -> case m of Nothing -> d; Just x -> f x ||]
test4 = [|| \x -> $$myMaybe 5 (\y -> y) (if (x :: Word32) `intEq` 0 then Just (1 :: Word32) else Nothing) ||]
-- *** Exception: Lft "Scope Error: cannot find 'find'"
test5 = [|| \x -> $$myMaybe 5 (\y -> y) (if (x :: Word32) `intEq` 0 then (find (\x -> x < 2) [2, 1::Word32]) else Nothing) ||]
