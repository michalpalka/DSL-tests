{-# LANGUAGE TemplateHaskell, QuasiQuotes, GADTs, DataKinds, FlexibleContexts, PatternSynonyms, PolyKinds #-}
module DSLDTest where

import Data.Word

import Language.Haskell.TH.Syntax (Q)
import qualified Language.Haskell.TH.Syntax (TExp)

import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import DSLD
import C

prop1 :: Maybe String -> Q (Language.Haskell.TH.Syntax.TExp Word32)
prop1 = propify prop1'

prop1' :: Maybe String -> Bool
prop1' (Just "NoData")   = False
prop1' (Just "HigherEd") = True
prop1' (Just "OtherEd")  = True
prop1' (Just "NoEd")     = True
prop1' Nothing           = False


-- To pretty-print
-- putDocLn $ ppr test1
-- To generate C code
-- putDocLn $ ppr $ tExpToC test1

test1 = toTExp eduMapQ prop1


