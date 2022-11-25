module BQLEvaluator () where

import BQLParser (Query, BType, Value, Exp, Statement)
import Data.Map (Map)
import Control.Monad.State (State)

data KVStore = Map String (Map String TypedVal)
data TypedVal = Typed Value BType

as :: Value -> BType -> TypedVal
v `as` t = Typed v t

evalExp :: Exp -> State KVStore TypedVal
evalExp = undefined

evalStatement :: Statement -> State KVStore (Maybe TypedVal)
evalStatement = undefined

evalQuery :: Query -> State KVStore (Maybe TypedVal)
evalQuery = undefined