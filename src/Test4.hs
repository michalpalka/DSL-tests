{-# LANGUAGE TemplateHaskell, GADTs, DataKinds, FlexibleContexts, PatternSynonyms #-}
module Test4 where

import Data.Word
import Control.Monad.State

import QHaskell hiding (get)
import QHaskell.Expression.Utils.Show.GADTFirstOrder ()

plus :: Float -> Float -> Float
plus = (+)

mul :: Float -> Float -> Float
mul = (*)

(***) :: Float -> Float -> Float
a *** b = a * b

makeQDSL "TestLang" ['plus, 'mul, '(***)]

pattern PlusVar   m n = Prm Zro (Ext m (Ext n Emp))
pattern MulVar    m n = Prm (Suc Zro) (Ext m (Ext n Emp))

testLang :: Qt Float -> ErrM Float
testLang q = do d <- translate q
                return (compile (normalise True d))


compile :: Type a => TestLang a -> a
compile = evaluate

myTest :: Qt (Float -> Float)
myTest = [|| \a -> mul (plus 2 1) a ||]


myApply :: Qt (Float -> Float) -> Qt Float -> Qt Float
myApply f x = [|| $$f $$x ||]

data TExp =
    Lit   Float
  | VarB  String
  | Fix   String TExp
  | Let   String TExp TExp
  | Fst   TExp
  | Snd   TExp
  | Pair  TExp TExp
  | Plus  TExp TExp
  | Mul   TExp TExp
  deriving (Eq, Show)

-- physical field + logical field + mapping
-- Income = NoData | IData Float
-- IncomePhy = -9 | -8 | -7 | 0 | *
--  -9, -8, -7, 0 -> NoData; x -> IData x
-- Education = NoData | HigherEd | OtherEd | NoEd
-- EducationPhy = -9 | -8 | -6 | 1 | 2 | 3
-- -9, -8, -6 -> NoData; 1 -> HigherEd; 2 -> OtherEd; 3 -> NoEd

-- 

data FieldT = FSum [Int] AtomT
  deriving (Eq, Show)

data AtomT = FInt | FFloat | FNone
  deriving (Eq, Show)

data LFieldT = LFT [(String, AtomT)]
  deriving (Eq, Show)

data Mapping = Mapping [([Int], String)] (Maybe String)
  deriving (Eq, Show)

incomePhy = FSum [-9, -8, -7, 0] FFloat

income = LFT [("NoData", FNone), ("IData", FFloat)]

incomeMap = Mapping [([-9, -8, -7, 0], "NoData")] (Just "IData")

data Atom = FVInt Int | FVFloat Float | FVNone
  deriving (Eq, Show)

data LField = LF String Atom
  deriving (Eq, Show)

type MapFun = Atom -> LField

makeMapFun :: Mapping -> MapFun
makeMapFun (Mapping l m) (FVInt i) = undefined
  
  



type NameMonad a = State Word32 a

newVar :: NameMonad Word32
newVar = do n <- get
            let n' = n + 1
            put n'
            return n'

-- | runs the given name monad
runNameMonad :: NameMonad a -> a
runNameMonad = flip evalState 0

toBackEnd :: TestLang a -> NameMonad TExp
toBackEnd l = case l of
  ConF     i   -> pure (Lit  i)
  PlusVar  m n -> toBackEnd2 Plus m n
  MulVar   m n -> toBackEnd2 Mul m n
  where
  toBackEnd2 :: (TExp -> TExp -> TExp) ->
                 TestLang a -> TestLang b -> NameMonad TExp
  toBackEnd2 c m n = c <$> toBackEnd m <*> toBackEnd n


test1 :: Qt Float
test1 = myApply myTest [|| 7 ||]

-- The type signatures require more imports
--myNorm
--  :: QHaskell.Singleton.HasSin QHaskell.Type.GADT.Typ a =>
--     Qt a -> TestLang a
myNorm ex =
  case fmap (normalise False) $ translate ex
  of Rgt e -> e

--runExample
--  :: QHaskell.Singleton.HasSin QHaskell.Type.GADT.Typ a =>
--     Qt a -> TExp
runExample ex =
  runNameMonad $ toBackEnd $ myNorm ex

-- run: runExample test1

hello = [|| 7 *** 4 :: Float ||]
