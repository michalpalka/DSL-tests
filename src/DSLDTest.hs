{-# LANGUAGE TemplateHaskell, QuasiQuotes, GADTs, DataKinds, FlexibleContexts, PatternSynonyms, PolyKinds #-}
module DSLDTest where

import Data.Word
import Control.Monad

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
  [[-9, -8, -6] |-> "UNK",
   [1]          |-> "ED5_6",
   [2]          |-> "ED3",
   [3]          |-> "ED2",
   [4]          |-> "ED0_1"]
  `noDefCase` ()

prop1 :: Maybe String -> Bool
prop1 (Just "UNK")      = False
prop1 (Just "ED5_6")    = True
prop1 (Just "ED3")      = True
prop1 (Just "ED2")      = True
prop1 (Just "ED0_1")    = True
prop1 Nothing           = False

prop2 :: Maybe String -> Bool
prop2 (Just "UNK") = False
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

genMainProp = genProp "test3.c" [("col1", test1)]


edu_table = marginalTable "edu_table.csv" ["isced97"]
pop_table = marginalTable "pop_table.csv" ["age", "sex"]

marginals = [edu_table, pop_table]

microSample = microSampleTable "Ghs06client.tab" ["Sex", "age", "EDLEV10", "GREARN"]

mappingAge = ranges
  [[0, 14]   |-> "Y_LT15",
   [15, 19]  |-> "Y15-19",
   [20, 24]  |-> "Y20-24",
   [25, 29]  |-> "Y25-29",
   [30, 34]  |-> "Y30-34",
   [35, 39]  |-> "Y35-39",
   [40, 44]  |-> "Y40-44",
   [45, 49]  |-> "Y45-49",
   [50, 54]  |-> "Y50-54",
   [55, 59]  |-> "Y55-59",
   [60, 64]  |-> "Y60-64",
   [65, 69]  |-> "Y65-69",
   [70, 74]  |-> "Y70-74",
   [75, 120] |-> "Y_GE75"
  ]

mappingSex :: Mapping
mappingSex =
  [[1]    |-> "M",
   [2]    |-> "F"]
  `noDefCase` ()

removeSpace l = [ c | c <- l, c /= ' ']

-- We can use a for loop to generate each separate program
mainSynPop :: IO ()
mainSynPop = do
    let pops = [generatePop
                (region ++ " ") marginals microSample
                (mappings [("Sex", "sex", mappingSex),
                           ("age", "age", mappingAge),
                           ("EDLEV10", "isced97", eduMapQ)])
                ["Sex", "age", "GRIND"]
                ("out_" ++ region)
               | region <- ["UKC11", "UKC12", "UKC13", "UKC14", "UKC21"]]
    doGenerate pops


-- 
-- 
-- 
-- 
-- 



