module SpruceEvaluator where

import Control.Applicative
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.Async (Async (..), async, wait)
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    newTVarIO,
    readTVar,
    readTVarIO,
    throwSTM,
    writeTVar,
  )
import Control.Monad (liftM)
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    guard,
    liftEither,
    mapExceptT,
    runExceptT,
  )
import Control.Monad.IO.Class
import Control.Monad.Identity
  ( Identity (runIdentity),
    runIdentityT,
  )
import Control.Monad.State
  ( MonadState (get, put),
    State,
    StateT (runStateT),
    gets,
    mapStateT,
  )
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.List (delete, find)
import Data.Map as Map
  ( Map,
    delete,
    empty,
    findWithDefault,
    fromAscList,
    fromList,
    insert,
    lookup,
  )
import Data.Maybe (isJust, isNothing)
import Data.Maybe qualified as Maybe
import ParseLib (Parser (doParse))
import ParseLib qualified as P
import SpruceParser
  ( PP (..),
    blockP,
    expP,
    programP,
    statementP,
  )
import SpruceTypes
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP

as :: Value -> BType -> TypedVal
v `as` t = Typed v t

typeof :: TypedVal -> BType
typeof (Typed v t) = t

instance PP TypedVal where
  pp (Typed v t) = pp v <> PP.text " : " <> pp t

-- Helper functions for dealing with scope
initScope :: ScopedStore
initScope = ScopedS {bindings = Map.empty, parent = Nothing}

emptyStore :: Store
emptyStore = St {vars = initScope, fdecls = Map.empty, shared = Map.empty, threads = []}

popLocalScope :: (MonadState Store m, MonadIO m) => m ()
popLocalScope = do
  store <- get
  let scoped = vars store
  case parent scoped of
    Just p -> put store {vars = p}
    Nothing -> error "Trying to unwind empty scope stack"

pushNewScope :: (MonadState Store m, MonadIO m) => m ()
pushNewScope = do
  store <- get
  let scopedVars = vars store
  let newScopedStore = ScopedS {parent = Just scopedVars, bindings = Map.empty}
  put store {vars = newScopedStore}

getGlobalScope :: (MonadState Store m, MonadIO m) => m ScopedStore
getGlobalScope = do gets (getListTail . vars)
  where
    getListTail :: ScopedStore -> ScopedStore
    getListTail s@(ScopedS {parent = Nothing}) = s
    getListTail (ScopedS {parent = Just s}) = getListTail s

getNonGlobalScope :: (MonadState Store m, MonadIO m) => m (Maybe ScopedStore)
getNonGlobalScope = do
  gets (aux . vars)
  where
    aux :: ScopedStore -> Maybe ScopedStore
    aux s@(ScopedS {parent = Nothing}) = Nothing
    aux s@(ScopedS {parent = Just p}) = Just $ s {parent = (aux p)}

appendToGlobalScope :: ScopedStore -> Maybe ScopedStore -> ScopedStore
appendToGlobalScope global Nothing = global
appendToGlobalScope global (Just local) =
  appendTail global local
  where
    appendTail :: ScopedStore -> ScopedStore -> ScopedStore
    appendTail toAdd s@(ScopedS {parent = Nothing}) = s {parent = Just toAdd}
    appendTail toAdd s@(ScopedS {parent = Just p}) = s {parent = Just (appendTail toAdd p)}

closureAddLVal :: TypedVal -> LValue -> TypedVal
closureAddLVal (Typed (FunctionClosure vdecls retTy body env _) t) loc =
  FunctionClosure vdecls retTy body env (Just loc) `as` t
closureAddLVal (Typed (ArrayVal vals) (ArrayT t)) loc =
  let typeAnnotatedVals = map (\x -> x `as` t) vals
      indexAnnotatedVals = zip [0 ..] typeAnnotatedVals
      -- error "hey"
      closureAnnotatedVals = map (\(i, tv) -> closureAddLVal tv (LArrInd loc (Val (IntVal i)))) indexAnnotatedVals
   in ArrayVal (map (\(Typed v t) -> v) closureAnnotatedVals) `as` (ArrayT t)
closureAddLVal tv _ = tv

-- Some abstractions that allow for code re-use between local and global variable stores
type GetStoreFunc m = m VarStore

type UpdateStoreFunc m = VarStore -> m ()

sharedGetStoreFunc :: (MonadState Store m, MonadIO m) => m TVarStore
sharedGetStoreFunc = gets shared

scopedGetSTM :: (MonadSTM m, MonadState Store m, MonadIO m) => LValue -> m (Maybe TypedVal)
scopedGetSTM = getTVarSTM sharedGetStoreFunc

scopedSetSTM :: (MonadSTM m, MonadState Store m, MonadIO m) => LValue -> TypedVal -> m ()
scopedSetSTM = setTVarSTM sharedGetStoreFunc

scopedRemoveSTM :: (MonadState Store m, MonadSTM m, MonadIO m) => LValue -> m ()
scopedRemoveSTM = removeVarSTM

scopedGet :: (MonadError String m, MonadState Store m, MonadIO m) => LValue -> m (Maybe TypedVal)
scopedGet loc = do
  shared <- getTVar sharedGetStoreFunc loc
  scoped <- getVar loc
  return (shared <|> scoped)

scopedSet :: (MonadError String m, MonadState Store m, MonadIO m) => LValue -> TypedVal -> m ()
scopedSet loc tv = do
  shared <- getTVar sharedGetStoreFunc loc
  if Maybe.isNothing shared
    then setVar loc tv
    else setTVar sharedGetStoreFunc loc tv

scopedCreateVar :: (MonadError String m, MonadState Store m, MonadIO m) => Bool -> LValue -> TypedVal -> m ()
scopedCreateVar True = setTVar sharedGetStoreFunc
scopedCreateVar False = createVar

-- Internal functions for setting/getting over an arbitrary VarStore
removeVar :: (MonadError String m, MonadState Store m, MonadIO m) => GetStoreFunc m -> UpdateStoreFunc m -> LValue -> m ()
removeVar getS updateS (LVar name) = do
  store <- getS
  updateS (Map.delete name store)
removeVar _ _ _ = error "Cannot remove arbitrary array variables"

getTVar :: (MonadError String m, MonadState Store m, MonadIO m) => m TVarStore -> LValue -> m (Maybe TypedVal)
getTVar getS (LVar name) = do
  store <- getS
  case Map.lookup name store of
    Nothing -> return Nothing
    Just tv -> liftIO $ Just <$> readTVarIO tv
getTVar _ _ = error "Shared arrays unsupported"

removeVarSTM :: (MonadSTM m, MonadState Store m, MonadIO m) => LValue -> m ()
removeVarSTM (LVar name) = do
  allVars <- get
  let store = shared allVars
  put $ allVars {shared = Map.delete name store}
removeVarSTM _ = error "Illegl remove"

getTVarSTM :: (MonadSTM m, MonadState Store m, MonadIO m) => m TVarStore -> LValue -> m (Maybe TypedVal)
getTVarSTM getS (LVar name) = do
  store <- getS
  case Map.lookup name store of
    Nothing -> return Nothing
    Just tv -> liftSTM $ Just <$> readTVar tv
getTVarSTM getS (LArrInd arr' ind') = do
  arrM <- getTVarSTM getS arr'
  let arr@(Typed arrV arrT) = Maybe.fromJust arrM
  ind@(Typed indV indT) <- evalExpSTM ind'
  case (arrV, indV, arrT) of
    (ArrayVal vals, IntVal i, ArrayT innerT) -> return $ Just (vals !! i `as` innerT)
    _ -> return Nothing

getVar :: (MonadError String m, MonadState Store m, MonadIO m) => LValue -> m (Maybe TypedVal)
getVar (LVar name) = do
  store <- get
  let scopes = vars store
  return (walkScopesList (Just scopes))
  where
    walkScopesList :: Maybe ScopedStore -> Maybe TypedVal
    walkScopesList Nothing = Nothing
    walkScopesList (Just s) = case Map.lookup name (bindings s) of
      Nothing -> walkScopesList (parent s)
      Just val -> Just val
getVar (LArrInd arr' ind') = do
  arrM <- getVar arr'
  arr@(Typed arrV arrT) <- extractMaybeOrError arrM "Cannot resolve array"
  typeGuardArr arr "Cannot index into non-array type"
  ind@(Typed indV indT) <- evalExp ind'
  typeGuard indT IntT "Index type must be an integer"
  case (arrV, indV, arrT) of
    (ArrayVal vals, IntVal i, ArrayT innerT) -> return $ Just (vals !! i `as` innerT)
    _ -> return Nothing

setTVarSTM :: (MonadSTM m, MonadState Store m, MonadIO m) => m TVarStore -> LValue -> TypedVal -> m ()
setTVarSTM getS (LVar name) val = do
  store <- getS
  case Map.lookup name store of
    Just x -> liftSTM $ writeTVar x val
    Nothing ->
      ( do
          tvar <- liftIO $ newTVarIO val
          let map = Map.insert name tvar store
          allVars <- get
          put allVars {shared = map}
          return ()
      )
setTVarSTM _ _ _ = error "Shared arrays unsupported"

setTVar :: (MonadError String m, MonadState Store m, MonadIO m) => m TVarStore -> LValue -> TypedVal -> m ()
setTVar getS (LVar name) val = do
  store <- getS
  case Map.lookup name store of
    Just x -> liftIO $ atomically $ writeTVar x val
    Nothing ->
      ( do
          tvar <- liftIO $ newTVarIO val
          let map = Map.insert name tvar store
          allVars <- get
          put allVars {shared = map}
          return ()
      )
setTVar _ _ _ = throwError "Shared arrays unsupported"

createVar :: (MonadError String m, MonadState Store m, MonadIO m) => LValue -> TypedVal -> m ()
createVar (LVar name) val = do
  store <- get
  let updatedBindings = Map.insert name val (bindings $ vars store)
  let updatedVars = (vars store) {bindings = updatedBindings}
  put store {vars = updatedVars}
createVar (LArrInd _ _) val = throwError "Cannot intialize single array element"

setVar :: (MonadError String m, MonadState Store m, MonadIO m) => LValue -> TypedVal -> m ()
setVar (LVar name) val = do
  store <- get
  let scopedStore = vars store
  let (updatedScopedStore, success) = walkSetScopeChain scopedStore
  if success
    then put store {vars = updatedScopedStore}
    else
      let updatedLocalBindings = Map.insert name val (bindings scopedStore)
          updatedStore = scopedStore {bindings = updatedLocalBindings}
       in put store {vars = updatedStore}
  where
    walkSetScopeChain :: ScopedStore -> (ScopedStore, Bool)
    walkSetScopeChain ss =
      let scopeVarBindings = bindings ss
          parentScope = parent ss
       in case Map.lookup name scopeVarBindings of
            Just _ -> (ScopedS (Map.insert name val scopeVarBindings) parentScope, True)
            Nothing -> case parentScope of
              Nothing -> (ss, False)
              Just pScope ->
                let (updatedParent, success) = walkSetScopeChain pScope
                 in (ScopedS scopeVarBindings (Just updatedParent), success)
setVar (LArrInd arr' ind') (Typed val valT) = do
  arrM <- getVar arr'

  arr@(Typed arrV arrT) <- extractMaybeOrError arrM "Cannot resolve variable"
  ind@(Typed indV indT) <- evalExp ind'

  typeGuard indT IntT "Index type must be an integer"
  typeGuardArr arr "Cannot index into non-array type"
  guardWithErrorMsg (ArrayT valT == arrT) "Inserting element of wrong type into array"

  case (arrV, indV) of
    (ArrayVal vals, IntVal i) ->
      if i < 0 || length vals <= i
        then throwError "Array index out of bounds"
        else setVar arr' (ArrayVal (setAtIndex vals i val) `as` arrT)
    _ -> error "ERR: Type system failure"
  where
    -- \| Credit to https://stackoverflow.com/questions/15530511/how-to-set-value-in-nth-element-in-a-haskell-list
    setAtIndex :: [a] -> Int -> a -> [a]
    setAtIndex l i v = Prelude.take i l ++ [v] ++ Prelude.drop (i + 1) l

-- Error Handling and Type Checking
typeOf :: Value -> Maybe BType
typeOf (BoolVal _) = Just BoolT
typeOf (IntVal _) = Just IntT
typeOf (StringVal _) = Just StringT
typeOf (ArrayVal []) = Just $ ArrayT AnyT
typeOf (ArrayVal [h]) = typeOf h
typeOf (ArrayVal (h : t)) = do
  hType <- typeOf h
  tType <- typeOf (ArrayVal t)
  case tType of
    ArrayT inner -> if inner == hType then Just tType else Nothing
    _ -> Nothing
typeOf (FunctionVal vdecls retTy block) =
  let argTypes = map (\(VDecl name _ ty) -> ty) vdecls
   in Just $ FuncT retTy argTypes
typeOf (FunctionClosure vdecls retTy block env _) =
  let argTypes = map (\(VDecl name _ ty) -> ty) vdecls
   in Just $ FuncT retTy argTypes

guardWithErrorMsg :: (MonadError String m, MonadState Store m, MonadIO m) => Bool -> String -> m ()
guardWithErrorMsg b m = do if b then return () else throwError m

extractMaybeOrError :: (MonadError String m, MonadState Store m, MonadIO m) => Maybe a -> String -> m a
extractMaybeOrError x msg = do
  case x of
    Nothing -> throwError msg
    Just y -> return y

-- Checks that the first type is a subtype of the second (equality except for AnyT case)
typeGuard :: (MonadError String m, MonadState Store m, MonadIO m) => BType -> BType -> String -> m ()
typeGuard _ AnyT m = do return ()
typeGuard (ArrayT t) (ArrayT t') m = typeGuard t t' m
typeGuard FuncAny (FuncT _ _) _ = do return ()
typeGuard t t' m = if t == t' then return () else throwError m

typeGuardArr :: (MonadError String m, MonadState Store m, MonadIO m) => TypedVal -> String -> m ()
typeGuardArr (Typed _ (ArrayT _)) _ = do return ()
typeGuardArr _ m = do throwError m

-- Expression evaluation helper functions
intBinOpToF :: Bop -> (Int -> Int -> Int)
intBinOpToF Mult = (*)
intBinOpToF Add = (+)
intBinOpToF Sub = (-)
intBinOpToF Div = div
intBinOpToF Mod = mod
intBinOpToF _ = error "ERR: Calling intBinOpToF with non-int op"

intCompToF :: Bop -> (Int -> Int -> Bool)
intCompToF Gt = (<)
intCompToF Ge = (<=)
intCompToF Eq = (==)
intCompToF NEq = (/=)
intCompToF Lt = (>)
intCompToF Le = (>=)
intCompToF _ = error "ERR: Calling intCompToF with non-comparison op"

boolOpToF :: Bop -> (Bool -> Bool -> Bool)
boolOpToF And = (&&)
boolOpToF Or = (||)
boolOpToF _ = error "ERR: Calling boolOpToF with non-bool op"

evalBinopIntExp :: (MonadError String m, MonadState Store m, MonadIO m) => Exp -> String -> m TypedVal
evalBinopIntExp (BOp op e1 e2) opName = do
  e1'@(Typed v1 t1) <- evalExp e1
  e2'@(Typed v2 t2) <- evalExp e2
  case (v1, v2) of
    (IntVal v1', IntVal v2') -> return $ IntVal (intBinOpToF op v1' v2') `as` IntT
    (StringVal v1', IntVal v2') -> case op of
      Add -> return $ StringVal (v1' ++ (show v2')) `as` StringT
      _ -> error "Cannot perform this operation on strings"
    (IntVal v1', StringVal v2') -> case op of
      Add -> return $ StringVal ((show v1') ++ v2') `as` StringT
      _ -> error "Cannot perform this operation on strings"
    (StringVal v1', StringVal v2') -> case op of
      Add -> return $ StringVal (v1' ++ v2') `as` StringT
      _ -> error "Cannot perform this operation on strings"
    _ -> error "Typeguard doesn't work as expected"
evalBinopIntExp _ _ = error "Calling evalBinop without binop exp"

evalBinopIntExpSTM :: (MonadSTM m, MonadState Store m, MonadIO m) => Exp -> String -> m TypedVal
evalBinopIntExpSTM (BOp op e1 e2) opName = do
  e1'@(Typed v1 t1) <- evalExpSTM e1
  e2'@(Typed v2 t2) <- evalExpSTM e2
  case (v1, v2) of
    (IntVal v1', IntVal v2') -> return $ IntVal (intBinOpToF op v1' v2') `as` IntT
    _ -> error "Typeguard doesn't work as expected"
evalBinopIntExpSTM _ _ = error "Calling evalBinop without binop exp"

evalCompExpSTM :: (MonadSTM m, MonadState Store m, MonadIO m) => Exp -> String -> m TypedVal
evalCompExpSTM (BOp op e1 e2) opName = do
  e1'@(Typed v1 t1) <- evalExpSTM e1
  e2'@(Typed v2 t2) <- evalExpSTM e2
  case (v1, v2) of
    (IntVal v1', IntVal v2') -> return $ BoolVal (intCompToF op v1' v2') `as` BoolT
    _ -> error "Typeguard doesn't work as expected"
evalCompExpSTM _ _ = error "Calling evalCompExp without comp exp"

evalCompExp :: (MonadError String m, MonadState Store m, MonadIO m) => Exp -> String -> m TypedVal
evalCompExp (BOp op e1 e2) opName = do
  e1'@(Typed v1 t1) <- evalExp e1
  typeGuard t1 IntT ("Arguments for " <> opName <> " must be integers")
  e2'@(Typed v2 t2) <- evalExp e2
  typeGuard t2 IntT ("Arguments for " <> opName <> " must be integers")
  case (v1, v2) of
    (IntVal v1', IntVal v2') -> return $ BoolVal (intCompToF op v1' v2') `as` BoolT
    _ -> error "Typeguard doesn't work as expected"
evalCompExp _ _ = error "Calling evalCompExp without comp exp"

evalBoolOpExpSTM :: (MonadSTM m, MonadState Store m, MonadIO m) => Exp -> String -> m TypedVal
evalBoolOpExpSTM (BOp op e1 e2) opName = do
  e1'@(Typed v1 t1) <- evalExpSTM e1
  e2'@(Typed v2 t2) <- evalExpSTM e2
  case (v1, v2) of
    (BoolVal v1', BoolVal v2') -> return $ BoolVal (boolOpToF op v1' v2') `as` BoolT
    _ -> error "Typeguard doesn't work as expected"
evalBoolOpExpSTM _ _ = error "Calling evalCompExp without comp exp"

evalBoolOpExp :: (MonadError String m, MonadState Store m, MonadIO m) => Exp -> String -> m TypedVal
evalBoolOpExp (BOp op e1 e2) opName = do
  e1'@(Typed v1 t1) <- evalExp e1
  typeGuard t1 BoolT ("Arguments for " <> opName <> " must be booleans")
  e2'@(Typed v2 t2) <- evalExp e2
  typeGuard t2 BoolT ("Arguments for " <> opName <> " must be booleans")
  case (v1, v2) of
    (BoolVal v1', BoolVal v2') -> return $ BoolVal (boolOpToF op v1' v2') `as` BoolT
    _ -> error "Typeguard doesn't work as expected"
evalBoolOpExp _ _ = error "Calling evalCompExp without comp exp"

evalExpSTM :: (MonadSTM m, MonadState Store m, MonadIO m) => Exp -> m TypedVal
evalExpSTM (Val v) = case typeOf v of
  Just t -> return $ v `as` t
  _ -> error $ "Type error" ++ (show v)
evalExpSTM (Var s) = do
  var <- scopedGetSTM (LVar s)
  case var of
    Just v -> return v
    Nothing -> error "Using undeclared variable"
evalExpSTM e@(BOp Add _ _) = evalBinopIntExpSTM e "addition"
evalExpSTM e@(BOp Sub _ _) = evalBinopIntExpSTM e "subtraction"
evalExpSTM e@(BOp Mod _ _) = evalBinopIntExpSTM e "modulo"
evalExpSTM e@(BOp Mult _ _) = evalBinopIntExpSTM e "multiplication"
evalExpSTM e@(BOp Div _ _) = evalBinopIntExpSTM e "division"
evalExpSTM e@(BOp Gt _ _) = evalCompExpSTM e "comparison"
evalExpSTM e@(BOp Ge _ _) = evalCompExpSTM e "comparison"
evalExpSTM e@(BOp Eq _ _) = evalCompExpSTM e "comparison"
evalExpSTM e@(BOp NEq _ _) = evalCompExpSTM e "comparison"
evalExpSTM e@(BOp Lt _ _) = evalCompExpSTM e "comparison"
evalExpSTM e@(BOp Le _ _) = evalCompExpSTM e "comparison"
evalExpSTM e@(BOp And _ _) = evalBoolOpExpSTM e "and"
evalExpSTM e@(BOp Or _ _) = evalBoolOpExpSTM e "or"
evalExpSTM (ArrInd arr' ind') = error "Shared arrays unsupported"
evalExpSTM (ArrCons _) = error "Shared arrays unsupported"
evalExpSTM (UOp Neg e) = do
  e1'@(Typed v1 t1) <- evalExpSTM e
  case v1 of
    IntVal v1' -> return $ IntVal (-v1') `as` IntT
    _ -> error "Typeguard doesn't work as expected"
evalExpSTM (UOp Not e) = do
  e1'@(Typed v1 t1) <- evalExpSTM e
  case v1 of
    BoolVal v1' -> return $ BoolVal (not v1') `as` BoolT
    _ -> error "Typeguard doesn't work as expected"
evalExpSTM (FCall name args) = error "You cannot call functions in shared memory"

-- Main logic for expression evaluation
evalExp :: forall m. (MonadError String m, MonadState Store m, MonadIO m) => Exp -> m TypedVal
evalExp (Val v@(FunctionVal vdecls retTy body)) = do
  currentLocalScope <- getNonGlobalScope
  case (currentLocalScope, typeOf v) of
    (Nothing, Just t) -> return $ FunctionClosure vdecls retTy body (ScopedS Map.empty Nothing) Nothing `as` t
    (Just s, Just t) -> return $ FunctionClosure vdecls retTy body s Nothing `as` t
    _ -> error "Internal error"
evalExp (Val v) = do
  case typeOf v of
    Just t -> return $ v `as` t
    _ -> throwError $ "Type error" ++ show v
evalExp (Var s) = do
  var <- scopedGet (LVar s)
  case var of
    Just v -> return v
    Nothing -> throwError $ "Use of undeclared variable " ++ s
evalExp e@(BOp Add _ _) = evalBinopIntExp e "addition"
evalExp e@(BOp Sub _ _) = evalBinopIntExp e "subtraction"
evalExp e@(BOp Mod _ _) = evalBinopIntExp e "modulo"
evalExp e@(BOp Mult _ _) = evalBinopIntExp e "multiplication"
evalExp e@(BOp Div _ _) = evalBinopIntExp e "division"
evalExp e@(BOp Gt _ _) = evalCompExp e "comparison"
evalExp e@(BOp Ge _ _) = evalCompExp e "comparison"
evalExp e@(BOp Eq _ _) = evalCompExp e "comparison"
evalExp e@(BOp NEq _ _) = evalCompExp e "comparison"
evalExp e@(BOp Lt _ _) = evalCompExp e "comparison"
evalExp e@(BOp Le _ _) = evalCompExp e "comparison"
evalExp e@(BOp And _ _) = evalBoolOpExp e "and"
evalExp e@(BOp Or _ _) = evalBoolOpExp e "or"
evalExp (ArrInd arr' ind') = do
  Typed arrV arrT <- evalExp arr'
  Typed indV indT <- evalExp ind'
  case (arrT, arrV, indT, indV) of
    (ArrayT innerT, ArrayVal vals, IntT, IntVal i) ->
      if i < 0 || length vals <= i
        then throwError "Index out of bounds"
        else return (vals !! i `as` innerT)
    (ArrayT _, _, _, _) -> throwError "Index must be an integer"
    (_, _, IntT, _) -> throwError "Attempted index into non-array type"
    _ -> error "ERR: type system internal error"
evalExp (ArrCons []) = do return (ArrayVal [] `as` ArrayT AnyT)
evalExp (ArrCons [h]) = do
  Typed v t <- evalExp h
  return $ ArrayVal [v] `as` ArrayT t
evalExp (ArrCons (h : t)) = do
  Typed hv ht <- evalExp h
  Typed tv tt <- evalExp $ ArrCons t
  case (tv, tt) of
    (ArrayVal tvals, ArrayT inner) ->
      if inner == ht
        then return $ ArrayVal (hv : tvals) `as` ArrayT inner
        else throwError "Type mismatch in array"
    _ -> error "ERR: Type system internal error"
evalExp (UOp Neg e) = do
  e1'@(Typed v1 t1) <- evalExp e
  typeGuard t1 IntT "Argument for negation must be an integer"
  case v1 of
    IntVal v1' -> return $ IntVal (-v1') `as` IntT
    _ -> error "Typeguard doesn't work as expected"
evalExp (UOp Not e) = do
  e1'@(Typed v1 t1) <- evalExp e
  typeGuard t1 BoolT "Argument for not must be a boolean"
  case v1 of
    BoolVal v1' -> return $ BoolVal (not v1') `as` BoolT
    _ -> error "Typeguard doesn't work as expected"
evalExp (FCall name args) =
  case libFuncLookup name of
    Just f -> do execLibFunc f args
    Nothing -> do execNamedUserFunc name args

extractFunctionExp :: (MonadError String m, MonadState Store m, MonadIO m) => TypedVal -> m ([VarDecl], BType, Block)
extractFunctionExp (Typed (FunctionVal vdecls retTy block) _) = do return (vdecls, retTy, block)
extractFunctionExp _ = throwError "ERR: Cannot call non-callable object"

execNamedUserFunc :: forall m. (MonadError String m, MonadState Store m, MonadIO m) => String -> [Exp] -> m TypedVal
execNamedUserFunc name args = do
  store <- get
  maybeFExp <- getVar (LVar name)
  fval <- extractMaybeOrError maybeFExp ("No function" ++ name)
  execUserFunc fval args

execUserFunc :: forall m. (MonadError String m, MonadState Store m, MonadIO m) => TypedVal -> [Exp] -> m TypedVal
execUserFunc (Typed (FunctionClosure argDecls expectedRetType body env locM) t) args = do
  store <- get
  localScopes <- getNonGlobalScope
  globalScope <- getGlobalScope
  argVals <- evalArgs args
  put store {vars = appendToGlobalScope globalScope (Just env)}
  pushNewScope
  setLocalArgBindings argDecls argVals
  ret <- evalBlock body
  updatedClosureEnvStoreM <- getNonGlobalScope
  let newEnv = newClosureEnv updatedClosureEnvStoreM
  let updatedClosure = FunctionClosure argDecls expectedRetType body newEnv locM
  case locM of
    Nothing -> return ()
    Just loc -> setVar loc (updatedClosure `as` t)
  newGlobalScope <- getGlobalScope
  let restoredVars = appendToGlobalScope newGlobalScope localScopes
  put store {vars = restoredVars}
  case (ret, expectedRetType) of
    (Just x@(Typed retVal retTy), _) -> return x
    (Nothing, VoidT) -> return (IntVal 0 `as` VoidT)
    (_, _) -> throwError "Return type for function doesn't match return value"
  where
    newClosureEnv :: Maybe ScopedStore -> ScopedStore
    newClosureEnv Nothing = ScopedS Map.empty Nothing
    newClosureEnv (Just s) = s

    evalArgs :: [Exp] -> m [TypedVal]
    evalArgs [] = do return []
    evalArgs (h : t) = do
      tv <- evalExp h
      tail <- evalArgs t
      return $ tv : tail

    setLocalArgBindings :: [VarDecl] -> [TypedVal] -> m ()
    setLocalArgBindings [] [] = do return ()
    setLocalArgBindings ((VDecl vname vshared vty) : declT) (tv@(Typed eVal eTy) : tvT) = do
      typeGuard eTy vty "Argument type doesn't match function declaration"
      setVar (LVar vname) tv
      setLocalArgBindings declT tvT
    setLocalArgBindings _ _ = do throwError "Argument mismatch in function call"
execUserFunc v _ = throwError $ "ERR: Attemtping to call non-callable object" <> (show v)

execLibFunc :: (MonadError String m, MonadState Store m, MonadIO m) => ([TypedVal] -> m TypedVal) -> [Exp] -> m TypedVal
execLibFunc f args = do
  argVals <- foldr aux (return []) args
  f argVals
  where
    aux :: (MonadError String m, MonadState Store m, MonadIO m) => Exp -> m [TypedVal] -> m [TypedVal]
    aux exp acc = do
      val <- evalExp exp
      rem <- acc
      return (val : rem)

-- Statement evaluation
evalStatement :: (MonadError String m, MonadState Store m, MonadIO m) => Statement -> m (Maybe TypedVal)
evalStatement (Let v@(VDecl name shared t) exp) = do
  exists <- scopedGet (LVar name)
  guardWithErrorMsg (isNothing exists) "Error: redeclaring variable"
  (Typed ev et) <- evalExp exp
  typeGuard t et ("Error: incorrect type in declaration for " ++ name)
  scopedCreateVar shared (LVar name) (closureAddLVal (ev `as` t) (LVar name))
  return Nothing
evalStatement (Assign lval exp) = do
  exists <- scopedGet lval
  (Typed currentV expectedT) <- extractMaybeOrError exists ("Error: assignment to undeclared variable" ++ show lval)
  e <- evalExp exp
  typeGuard (typeof e) expectedT "Error: Assignment must respect the use the same type"
  scopedSet lval (closureAddLVal e lval)
  return Nothing
evalStatement (Return exp) = do
  tv <- evalExp exp
  return $ Just tv
evalStatement (FCallStatement name args) = do
  e <- evalExp (FCall name args)
  return Nothing
evalStatement (If exp b1 b2) = do
  e <- evalExp exp
  typeGuard (typeof e) BoolT "If-else expression guard must be of type bool"
  pushNewScope
  x <- case e of
    Typed (BoolVal b) BoolT ->
      if b
        then evalBlock b1
        else evalBlock b2
    _ -> error "ERR: Type system internal error"
  popLocalScope
  return x
evalStatement (While exp body) = do
  e@(Typed expV expT) <- evalExp exp
  typeGuard (typeof e) BoolT "Error: guard of while loop must be boolean type"
  case expV of
    BoolVal b ->
      if b
        then do
          pushNewScope
          val <- evalBlock body
          -- store <- get
          -- error (show (vars store))
          popLocalScope
          case val of
            Nothing -> evalStatement (While exp body)
            Just x -> return $ Just x
        else return Nothing
    _ -> error "ERR: Type system internal error"
evalStatement (Atomic block) = do
  allVars <- get
  (value, store) <- liftIO $ atomically (runStateT (evalBlockSTM block) allVars)
  return value
evalStatement (ForIn vdecl@(VDecl n b t) exp body) = do
  v@(Typed expV expT) <- evalExp exp
  case (expT, expV) of
    (ArrayT innerT, ArrayVal vals) ->
      if t /= innerT
        then throwError "Error: Variable delcaration in for-each doesn't match expression"
        else execForEach vdecl vals body
    _ -> throwError "Error: cannot iterate over non-array type"
  where
    execForEach :: (MonadError String m, MonadState Store m, MonadIO m) => VarDecl -> [Value] -> Block -> m (Maybe TypedVal)
    execForEach vd@(VDecl iterName shared iterType) (h : t) body = do
      pushNewScope
      createVar (LVar iterName) (h `as` iterType)
      res <- evalBlock body
      popLocalScope
      case res of
        Nothing -> execForEach vd t body
        Just x -> return $ Just x
    execForEach _ [] _ = do return Nothing

evalStatementSTM :: (MonadSTM m, MonadState Store m, MonadIO m) => Statement -> m (Maybe TypedVal)
evalStatementSTM (Let v@(VDecl name shared t) exp) = do
  exists <- scopedGetSTM (LVar name)
  (Typed ev et) <- evalExpSTM exp
  scopedSetSTM (LVar name) (ev `as` t)
  return Nothing
evalStatementSTM (Assign lval exp) = do
  exists <- scopedGetSTM lval
  let (Typed currentV expectedT) = Maybe.fromJust exists
  e <- evalExpSTM exp
  scopedSetSTM lval e
  return Nothing
evalStatementSTM (Return exp) = do
  tv <- evalExpSTM exp
  return $ Just tv
evalStatementSTM (FCallStatement name args) = do
  e <- evalExpSTM (FCall name args)
  return Nothing
evalStatementSTM (If exp b1 b2) = do
  e <- evalExpSTM exp
  pushNewScope
  tmp <- case e of
    Typed (BoolVal b) BoolT ->
      if b
        then evalBlockSTM b1
        else evalBlockSTM b2
    _ -> error "ERR: Type system internal error"
  popLocalScope
  return tmp
evalStatementSTM (While exp body) = do
  e@(Typed expV expT) <- evalExpSTM exp
  case expV of
    BoolVal b ->
      if b
        then do
          pushNewScope
          val <- evalBlockSTM body
          popLocalScope
          case val of
            Nothing -> evalStatementSTM (While exp body)
            Just x -> return $ Just x
        else return Nothing
    _ -> error "ERR: Type system internal error"
evalStatementSTM (Atomic block) = error "Calling atomic within atomic"
evalStatementSTM (ForIn vdecl@(VDecl n b t) exp body) = do
  v@(Typed expV expT) <- evalExpSTM exp
  case (expT, expV) of
    (ArrayT innerT, ArrayVal vals) ->
      if t /= innerT
        then error "Error: Variable delcaration in for-each doesn't match expression"
        else execForEach vdecl vals body
    _ -> error "Error: cannot iterate over non-array type"
  where
    execForEach :: (MonadSTM m, MonadState Store m, MonadIO m) => VarDecl -> [Value] -> Block -> m (Maybe TypedVal)
    execForEach vd@(VDecl iterName shared iterType) (h : t) body = do
      scopedSetSTM (LVar iterName) (h `as` iterType)
      res <- evalBlockSTM body
      scopedRemoveSTM (LVar iterName)
      case res of
        Nothing -> execForEach vd t body
        Just x -> return (Just x)
    execForEach _ [] _ = do return Nothing

evalBlockSTM :: (MonadSTM m, MonadState Store m, MonadIO m) => Block -> m (Maybe TypedVal)
evalBlockSTM (Block []) = return Nothing
evalBlockSTM (Block (h : t)) = do
  res <- evalStatementSTM h
  case res of
    Nothing -> evalBlockSTM (Block t)
    _ -> return res

evalBlock :: (MonadError String m, MonadState Store m, MonadIO m) => Block -> m (Maybe TypedVal)
evalBlock (Block []) = do return Nothing
evalBlock (Block (h : t)) = do
  res <- evalStatement h
  case res of
    Nothing -> evalBlock (Block t)
    _ -> return res

evalProgram :: (MonadError String m, MonadState Store m, MonadIO m) => Program -> m (Maybe TypedVal)
evalProgram (Program fdecls main) = do
  let initBindings = Map.fromList (map aux fdecls)
      initScopedVars = ScopedS {bindings = initBindings, parent = Nothing}
      initStore = emptyStore {vars = initScopedVars}
   in do
        put initStore
        evalBlock main
  where
    aux :: FDecl -> (String, TypedVal)
    aux (FDecl name vdecls retTy block) =
      let argTypes = map (\(VDecl _ _ argTy) -> argTy) vdecls
          fType = FuncT retTy argTypes
          fValue = FunctionClosure vdecls retTy block (ScopedS Map.empty Nothing) Nothing
       in (name, fValue `as` fType)

-- Library Functions
libFuncLookup :: (MonadError String m, MonadState Store m, MonadIO m) => String -> Maybe ([TypedVal] -> m TypedVal)
libFuncLookup "appendFront" = Just libAppendFront
libFuncLookup "appendBack" = Just libAppendBack
libFuncLookup "len" = Just libArrayLen
libFuncLookup "range" = Just libRange
libFuncLookup "fork" = Just libFork
libFuncLookup "wait" = Just libWait
libFuncLookup "print" = Just libPrint
libFuncLookup x = Nothing

guardTypes :: (MonadError String m, MonadState Store m, MonadIO m) => [TypedVal] -> [BType] -> String -> m ()
guardTypes [] [] fname = do return ()
guardTypes ((Typed v t) : valT) (tyH : tyT) fname = do
  typeGuard t tyH ("Mismatched type in function call for " ++ fname)
  guardTypes valT tyT fname
guardTypes _ _ fname = do throwError ("Mismatched number of arguments in " ++ fname)

libAppendFront :: (MonadError String m, MonadState Store m, MonadIO m) => [TypedVal] -> m TypedVal
libAppendFront args = do
  guardTypes args [AnyT, ArrayT AnyT] "appendFront"
  let (Typed v1 t1) = args !! 0
      (Typed v2 t2) = args !! 1
  case (t1, t2, v1, v2) of
    (t, ArrayT t', toAppend, ArrayVal vals) -> do
      typeGuard t t' "Error: append must respect array type"
      return $ ArrayVal (toAppend : vals) `as` ArrayT t1
    _ -> throwError "e"

libAppendBack :: (MonadError String m, MonadState Store m, MonadIO m) => [TypedVal] -> m TypedVal
libAppendBack args = do
  guardTypes args [AnyT, ArrayT AnyT] "appendFront"
  let (Typed v1 t1) = args !! 0
      (Typed v2 t2) = args !! 1
  case (t1, t2, v1, v2) of
    (t, ArrayT t', toAppend, ArrayVal vals) -> do
      typeGuard t t' "Error: append must respect array type"
      return $ ArrayVal (vals ++ [toAppend]) `as` ArrayT t1
    _ -> throwError "ERR: Type system internal failure"

libArrayLen :: (MonadError String m, MonadState Store m, MonadIO m) => [TypedVal] -> m TypedVal
libArrayLen args = do
  guardTypes args [ArrayT AnyT] "len"
  let (Typed v1 t1) = args !! 0
  case (t1, v1) of
    (ArrayT innerT, ArrayVal vals) -> return $ IntVal (length vals) `as` IntT
    _ -> throwError "ERR: Type system internal failure"

libRange :: (MonadError String m, MonadState Store m, MonadIO m) => [TypedVal] -> m TypedVal
libRange args = do
  guardTypes args [IntT] "range"
  let (Typed v1 t1) = args !! 0
  case (t1, v1) of
    (IntT, IntVal i) -> return $ (ArrayVal (IntVal <$> take i [0 ..]) `as` ArrayT IntT)
    _ -> throwError "ERR: Type system internal failure"

execIO :: (MonadIO m) => TypedVal -> Store -> m ()
execIO fval store =
  do
    (evalRes, finalStore) <- runStateT (runExceptT (execUserFunc fval [])) store
    case evalRes of
      Left evalErr -> error ("Forked function failed: " <> evalErr)
      Right res -> return ()

libFork :: (MonadError String m, MonadState Store m, MonadIO m) => [TypedVal] -> m TypedVal
libFork args = do
  -- TODO : Type guards
  let typedFVal@(Typed v1 t1) = args !! 0
  case (t1, v1) of
    (FuncT _ _, FunctionClosure vdecls retTy block _ _) -> do
      allVars <- get
      async <- liftIO $ async (execIO typedFVal allVars)
      let threads' = async : threads allVars
      put allVars {threads = threads'}
      return $ StringVal (show $ asyncThreadId async) `as` StringT
    _ -> throwError "ERR: Type system internal failure"

libWait :: (MonadError String m, MonadState Store m, MonadIO m) => [TypedVal] -> m TypedVal
libWait args = do
  guardTypes args [StringT] "wait"
  let (Typed v1 t1) = args !! 0
  case (t1, v1) of
    (StringT, StringVal s) -> do
      allVars <- get
      let threads' = threads allVars
      case find (\a -> show (asyncThreadId a) == s) threads' of
        Just match -> do
          liftIO $ wait match
          put allVars {threads = Data.List.delete match threads'}
          return $ IntVal 0 `as` VoidT
        Nothing -> throwError "Waiting for non existent thread!"
    _ -> throwError "ERR: Type system internal failure"

libPrint :: (MonadError String m, MonadState Store m, MonadIO m) => [TypedVal] -> m TypedVal
libPrint args = do
  let (Typed v1 t1) = args !! 0
  case (t1, v1) of
    (BoolT, BoolVal b) -> do
      liftIO $ print b
      return $ IntVal 0 `as` VoidT
    (IntT, IntVal i) -> do
      liftIO $ print i
      return $ IntVal 0 `as` VoidT
    (StringT, StringVal s) -> do
      liftIO $ print s
      return $ IntVal 0 `as` VoidT
    _ -> throwError "Can't print array types"

evalProgramFile :: String -> IO (Either String (Maybe TypedVal))
evalProgramFile fname = do
  parseResM <- P.parseFromFile programP fname
  case parseResM of
    Left parseErr -> return $ Left parseErr
    Right parsedProgram -> do
      (value, finalStore) <- runStateT (runExceptT (evalProgram parsedProgram)) emptyStore
      return value
