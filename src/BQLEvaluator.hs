module BQLEvaluator () where

import BQLParser (Query, BType, Value, Exp, Statement)
import Data.Map (Map)
import Control.Monad.State (State)

type KVStore = Map String (Map String TypedVal)
type VarStore = Map String TypedVal
data TypedVal = Typed Value BType

as :: Value -> BType -> TypedVal
v `as` t = Typed v t


----- Code executes against a function-local scope, global scope, and persistent KV-store
data Store = S {
    locals :: VarStore,
    globals :: VarStore,
    persist :: KVStore
}

getVar :: String -> State Store TypedVal
getVar = undefined

setLocal :: String -> TypedVal -> State Store ()
setLocal = undefined

setGlobal :: String -> TypedVal -> State Store ()
setGlobal = undefined

getKV :: String -> String -> State Store TypedVal
getKV = undefined

setKV :: String -> String -> TypedVal -> State Store ()
setKV = undefined


----- Expression evaluation
evalExp :: Exp -> State Store TypedVal
evalExp = undefined

evalStatement :: Statement -> State Store (Maybe TypedVal)
evalStatement = undefined

evalQuery :: Query -> State Store (Maybe TypedVal)
evalQuery = undefined