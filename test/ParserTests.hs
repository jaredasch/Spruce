module ParserTests where

import BQLParser qualified as BP
import ParseLib qualified as P
import Test.QuickCheck as QC
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP

pretty :: BP.PP a => a -> String
pretty = PP.render . BP.pp

prop_roundtrip_val :: BP.Value -> Bool
prop_roundtrip_val v = P.parse BP.valueP (pretty v) == Right v

prop_roundtrip_exp :: BP.Exp -> Bool
prop_roundtrip_exp e = P.parse BP.expP (pretty e) == Right e

prop_roundtrip_statement :: BP.Statement -> Bool
prop_roundtrip_statement s = P.parse BP.statementP (pretty s) == Right s

parserQC :: IO ()
parserQC = do
  putStrLn "\nRountrip value testing..."
  QC.quickCheck prop_roundtrip_val
  putStrLn "Rountrip expression parsing testing..."
  QC.quickCheck prop_roundtrip_exp
  putStrLn "Roundtrip statement parsing testing..."
  QC.quickCheck prop_roundtrip_statement
