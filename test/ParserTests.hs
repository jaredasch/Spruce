module ParserTests where

import ParseLib qualified as P
import SpruceParser qualified as BP
import SpruceTypes qualified as BT
import Test.QuickCheck as QC
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP

pretty :: BP.PP a => a -> String
pretty = PP.render . BP.pp

prop_roundtrip_val :: BT.Value -> Bool
prop_roundtrip_val v = P.parse BP.valueP (pretty v) == Right v

prop_roundtrip_exp :: BT.Exp -> Bool
prop_roundtrip_exp e = P.parse BP.expP (pretty e) == Right e

prop_roundtrip_statement :: BT.Statement -> Bool
prop_roundtrip_statement s = P.parse BP.statementP (pretty s) == Right s

parserQC :: IO ()
parserQC = do
  putStrLn "\nRountrip value testing..."
  QC.quickCheck prop_roundtrip_val
  putStrLn "Rountrip expression parsing testing..."
  QC.quickCheck prop_roundtrip_exp

-- putStrLn "Roundtrip statement parsing testing..."
-- QC.quickCheck prop_roundtrip_statement
