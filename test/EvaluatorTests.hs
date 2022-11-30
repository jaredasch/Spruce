module EvaluatorTests where

import BQLEvaluator (TypedVal, as)
import BQLEvaluator qualified as BE
import BQLParser (BType (..), Value (..))
import BQLParser qualified as BP
import Test.HUnit (Test (TestList), runTestTT, (~:), (~?=))

returnVal :: TypedVal -> Either String (Maybe TypedVal)
returnVal = undefined

returnVoid :: Either String (Maybe TypedVal)
returnVoid = Right Nothing

evalError :: String -> Either String (Maybe TypedVal)
evalError = Left

testFile :: String -> Either String (Maybe TypedVal) -> Test
testFile filename expected = undefined