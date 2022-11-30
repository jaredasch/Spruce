import EvaluatorTests (evalTestSuite)
import ParserTests (parserQC)
import Test.HUnit (Assertion, Test (TestList), runTestTT, (~:), (~?=))

main :: IO ()
main = do
  parserQC
  runTestTT evalTestSuite
  return ()