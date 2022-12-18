module BQLTypes where

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (STM, TVar)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Map as Map (Map)

-- VDecl <varname> <shared> <type>
data VarDecl = VDecl String Bool BType deriving (Show, Eq)

data BType
  = VoidT
  | BoolT
  | IntT
  | StringT
  | ArrayT BType
  | FuncAny -- Used for parsing
  | FuncT BType [BType] -- Used for internal type checking
  | AnyT -- Only to be used internally with empty arrays, can't be used by users
  deriving (Show, Eq)

-- | The first argument contains all function declarations, the second is the main
-- code to be executed
data Query = Query [FDecl] Block deriving (Show, Eq)

data FDecl = FDecl String [VarDecl] BType Block deriving (Show, Eq)

newtype Block = Block [Statement] deriving (Show, Eq)

data Statement
  = Assign LValue Exp
  | Let VarDecl Exp
  | If Exp Block Block
  | Return Exp
  | FCallStatement String [Exp]
  | While Exp Block
  | ForIn VarDecl Exp Block
  | Atomic Block
  deriving (Show, Eq)

data Exp
  = Val Value
  | Var String
  | ArrInd Exp Exp
  | ArrCons [Exp]
  | BOp Bop Exp Exp
  | UOp Uop Exp
  | FCall String [Exp]
  deriving (Show, Eq)

data Value
  = BoolVal Bool
  | IntVal Int
  | StringVal String
  | ArrayVal [Value]
  | FunctionVal [VarDecl] BType Block
  | FunctionClosure [VarDecl] BType Block ScopedStore (Maybe LValue)
  deriving (Show, Eq)

data LValue
  = LVar String
  | LArrInd LValue Exp
  deriving (Show, Eq)

data Bop
  = Add
  | Sub
  | Mult
  | Div
  | Mod
  | And
  | Or
  | Gt
  | Ge
  | Eq
  | NEq
  | Lt
  | Le
  deriving (Show, Eq)

data Uop
  = Neg
  | Not
  deriving (Show, Eq)

class MonadSTM m where liftSTM :: STM a -> m a

instance MonadIO STM where liftIO = undefined

instance MonadSTM STM where liftSTM = id

instance (Monad m, MonadSTM m, MonadTrans t) => MonadSTM (t m) where
  liftSTM = lift . liftSTM

-- Map type for row-column-based key-value store
type KVStore = Map String (Map String TypedVal)

-- Map type for local/global variables
type VarStore = Map String TypedVal

-- Map type for transactional variables
type TVarStore = Map String (TVar TypedVal)

-- Function table to store user-defined functions
type FunctionTable = Map String FDecl

data Scope = Global | Local deriving (Show)

-- Data type for storing typed values
data TypedVal = Typed Value BType deriving (Show, Eq)

data Store = St
  { vars :: ScopedStore,
    shared :: TVarStore,
    fdecls :: FunctionTable,
    threads :: [Async ()]
  }

-- Types for scope
data ScopedStore = ScopedS
  { bindings :: VarStore,
    parent :: Maybe ScopedStore
  }
  deriving (Show, Eq)
