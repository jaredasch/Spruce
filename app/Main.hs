module Main where

import ParseLib
import BQLParser
import BQLEvaluator
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

main :: IO ()
main = do
  let actions = do
        t <- newTVar (3 :: Integer)
        writeTVar t 4
        readTVar t
  x <- atomically actions
  putStrLn $ "Hello world" ++ show x
