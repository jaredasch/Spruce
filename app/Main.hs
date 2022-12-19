module Main where

import GHC.IO.Handle (hFlush)
import ParseLib
import SpruceEvaluator (evalProgramFile)
import SpruceParser
import System.IO

main :: IO ()
main = do
  fname <- putStr "spruce> " *> hFlush stdout *> getLine
  if fname == ":q"
    then return ()
    else do
      result <- evalProgramFile fname
      case result of
        Left err -> putStrLn err
        Right (Just returnVal) -> print returnVal
        Right Nothing -> putStrLn "Success: no return value"
      main
