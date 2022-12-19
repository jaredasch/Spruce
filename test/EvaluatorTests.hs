module EvaluatorTests where

import BQLEvaluator (as)
import BQLEvaluator qualified as BE
import BQLParser qualified as BP
import BQLTypes (BType (..), TypedVal, Value (..))
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
  actual <- BE.evalProgramFile fname
  if actual == expected
    then return ()
    else assertFailure $ fileTestErrorMsg fname expected actual

($=$) :: String -> Either String (Maybe TypedVal) -> Test
fname $=$ expected = TestCase (assertFileResultEq fname expected)

evalTestSuite :: Test
evalTestSuite =
  TestList
    [ "test/queries/foreach.spr" $=$ int 55,
      "test/queries/global_local_naming_correct.spr" $=$ int 2,
      "test/queries/badly_typed_arr.spr" $=$ evalError "Type mismatch in array",
      "test/queries/global_local_naming_conflict.spr" $=$ evalError "Error: redeclaring variable",
      "test/queries/nested_if.spr" $=$ int (-1),
      "test/queries/bad_scoping.spr" $=$ evalError "Use of undeclared variable i",
      "test/queries/fib.spr" $=$ returnVal (ArrayVal (IntVal <$> [10946, 17711, 28657]) `as` ArrayT IntT),
      "test/queries/memoized_fib.spr" $=$ returnVal (ArrayVal (IntVal <$> [10946, 17711, 28657]) `as` ArrayT IntT),
      "test/demo/01_types.spr" $=$ int 55,
      "test/demo/02_arrays.spr" $=$ returnVal (ArrayVal (IntVal <$> [0, 1, 3, 6, 10]) `as` ArrayT IntT),
      "test/demo/03_functions.spr" $=$ returnVal (ArrayVal (IntVal <$> [0, 1, 3, 6, 10]) `as` ArrayT IntT),
      "test/demo/04_badly_typed.spr" $=$ evalError "Type mismatch in array",
      "test/demo/05_forkwait.spr" $=$ int 499500,
      "test/demo/07_atomic.spr" $=$ int 6000,
      "test/demo/09_firstclassfuncs.spr" $=$ int 2,
      "test/demo/10_scoping_complex.spr" $=$ int 165,
      "test/demo/11_closures.spr" $=$ int 3,
      "test/demo/12_closures_array.spr" $=$ int 2
    ]
