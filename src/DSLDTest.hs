{-# LANGUAGE TemplateHaskell, QuasiQuotes, GADTs, DataKinds, FlexibleContexts, PatternSynonyms, PolyKinds #-}
module DSLDTest where

import Data.Word

import Language.Haskell.TH.Syntax (Q)
import qualified Language.Haskell.TH.Syntax (TExp)

import qualified Language.C.Syntax as CSyntax

import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import System.IO

import DSLD
import C

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

prop1 :: Maybe String -> Bool
prop1 (Just "NoData")   = False
prop1 (Just "HigherEd") = True
prop1 (Just "OtherEd")  = True
prop1 (Just "NoEd")     = True
prop1 Nothing           = False

prop2 :: Maybe String -> Bool
prop2 (Just "NoData") = False
prop2 (Just "IData")  = True
prop2 Nothing         = False


test1 = toTExp eduMapQ prop1

-- To pretty-print
test2 = putDocLn $ ppr test1

-- To generate C code
test3 = putDocLn $ ppr $ tExpToC test1

itest = toTExp incomeMapQ prop2

genMain = do
  withFile "test2.c" WriteMode $ \h -> do
    hPutDocLn h $ ppr mainReadCSV

genMainProp = do
  withFile "test3.c" WriteMode $ \h -> do
    hPutDocLn h $ ppr $ mainReadCSVProp [(1, tExpToCSVProp test1)]

toCSVProp :: (Mapping, Maybe String -> Bool) -> CSyntax.Stm
toCSVProp (m,p) = tExpToCSVProp $ toTExp m p

checkProps filename fields = do
  withFile filename WriteMode $ \h -> do
    hPutDocLn h $ ppr $ mainReadCSVProp fields

checkIProp = checkProps "itest.c" [(2, toCSVProp (incomeMapQ, prop2))]
