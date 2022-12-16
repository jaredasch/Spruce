module EvaluatorTests where

import BQLEvaluator (TypedVal, as)
import BQLEvaluator qualified as BE
import BQLParser (BType (..), Value (..))
import BQLParser qualified as BP
import Test.HUnit (Assertion, Test (TestCase, TestList), assertFailure, runTestTT, (~:), (~?=))
import Text.PrettyPrint (render, text)

type EvalResult = Either String (Maybe TypedVal)

pretty :: BP.PP a => a -> String
pretty = render . BP.pp

instance BP.PP EvalResult where
  pp (Right (Just x)) = text "success: " <> BP.pp x
  pp (Right Nothing) = text "success: void"
  pp (Left err) = text $ "error " <> err

returnVal :: TypedVal -> EvalResult
returnVal = Right . Just

returnVoid :: EvalResult
returnVoid = Right Nothing

evalError :: String -> EvalResult
evalError = Left

testFile :: String -> Either String (Maybe TypedVal) -> Test
testFile filename expected = undefined

-- Helpers to convert Haskell types to result types
void :: EvalResult
void = returnVoid

bool :: Bool -> EvalResult
bool b = returnVal (BoolVal b `as` BoolT)

int :: Int -> EvalResult
int i = returnVal (IntVal i `as` IntT)

string :: String -> EvalResult
string s = returnVal (StringVal s `as` StringT)

-- Perform tests
fileTestErrorMsg :: String -> EvalResult -> EvalResult -> String
fileTestErrorMsg tname expected actual =
  "Error: test "
    <> tname
    <> " returned "
    <> pretty actual
    <> ", but expected "
    <> pretty expected

assertFileResultEq :: String -> Either String (Maybe TypedVal) -> Assertion
assertFileResultEq fname expected = do
  actual <- BE.evalQueryFile fname
  if actual == expected
    then return ()
    else assertFailure $ fileTestErrorMsg fname expected actual

($=$) :: String -> Either String (Maybe TypedVal) -> Test
fname $=$ expected = TestCase (assertFileResultEq fname expected)

evalTestSuite :: Test
evalTestSuite =
  TestList
    -- [ "test/queries/foreach.bql" $=$ int 55,
    --   "test/queries/global_local_naming_correct.bql" $=$ int 2,
    --   "test/queries/badly_typed_arr.bql" $=$ evalError "Type mismatch in array",
    --   "test/queries/global_local_naming_conflict.bql" $=$ evalError "Error: redeclaring variable"
    -- ]
    [ "test/demo/1_types.bql" $=$ int 55,
      "test/demo/2_arrays.bql" $=$ returnVal (ArrayVal (IntVal <$> [0, 1, 3, 6, 10]) `as` ArrayT IntT),
      "test/demo/3_functions.bql" $=$ returnVal (ArrayVal (IntVal <$> [0, 1, 3, 6, 10]) `as` ArrayT IntT),
      "test/demo/4_badly_typed.bql" $=$ evalError "Type mismatch in array",
      "test/demo/5_first_class.bql" $=$ returnVal (IntVal 10 `as` IntT)
    ]
