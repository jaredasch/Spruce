module BQLEvaluator where

import BQLParser
    ( Query(..),
      Statement(..),
      Exp(..),
      BType(..),
      Value(..),
      Bop(..),
      Uop(..),
      PP(..),
      LValue(..),
      VarDecl(..),
      Block(..),
      FDecl(..),
      expP, statementP, blockP, queryP, Uop (Neg), LValue )
import Data.Map as Map
    ( empty, findWithDefault, insert, lookup, Map, fromAscList )
import Control.Applicative
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    runExceptT, guard,
  )
import Control.Monad.Identity
  ( Identity (runIdentity),
  )
import Control.Monad.State
  ( MonadState (get, put),
    StateT (runStateT),
    State,
    gets,
  )
import ParseLib (Parser(doParse))
import Text.PrettyPrint (Doc, (<+>))
import qualified Text.PrettyPrint as PP
import Data.Maybe (isNothing, isJust)

type KVStore = Map String (Map String TypedVal)
type VarStore = Map String TypedVal
type FunctionTable = Map String FDecl
data Scope = Global | Local

----- Data type for storing typed values
data TypedVal = Typed Value BType deriving (Show)

instance PP TypedVal where
    pp (Typed t v) = pp v <> PP.text " : " <> pp t 

infixl 9 `as`
as :: Value -> BType -> TypedVal
v `as` t = Typed v t

----- Code executes against a function-local scope, global scope, and persistent KV-store
data Store = St {
    locals :: VarStore,
    globals :: VarStore,
    persist :: KVStore,
    fdecls :: FunctionTable
} deriving (Show)
emptyStore :: Store
emptyStore = St {locals=Map.empty, globals=Map.empty, persist=Map.empty, fdecls=Map.empty}

type GetStoreFunc m = m VarStore
type UpdateStoreFunc m = VarStore -> m ()

localGetStoreFunc :: (MonadError String m, MonadState Store m) => m VarStore
localGetStoreFunc = gets locals

localUpdateStoreFunc :: (MonadError String m, MonadState Store m) => VarStore -> m ()
localUpdateStoreFunc v = do
    allVars <- get 
    put allVars {locals=v}

scopedGet :: (MonadError String m, MonadState Store m) => Scope -> LValue -> m (Maybe TypedVal)
scopedGet Global = getGlobal
scopedGet Local = getLocal

scopedSet :: (MonadError String m, MonadState Store m) => Scope -> LValue -> TypedVal -> m ()
scopedSet Global = setGlobal
scopedSet Local = setLocal

getLocal :: (MonadError String m, MonadState Store m) => LValue -> m (Maybe TypedVal)
getLocal l = do
    local <- getVar localGetStoreFunc l
    global <- getVar globalGetStoreFunc l
    return (local <|> global)

setLocal :: (MonadError String m, MonadState Store m) => LValue -> TypedVal -> m ()
setLocal loc v = do 
    existsGlobal <- getVar globalGetStoreFunc loc
    case existsGlobal of
        Just x -> setVar globalGetStoreFunc globalUpdateStoreFunc loc v
        Nothing -> setVar localGetStoreFunc localUpdateStoreFunc loc v

globalGetStoreFunc :: (MonadError String m, MonadState Store m) => m VarStore
globalGetStoreFunc = gets globals

globalUpdateStoreFunc :: (MonadError String m, MonadState Store m) => VarStore -> m ()
globalUpdateStoreFunc v = do
    allVars <- get 
    put allVars {globals=v}

getGlobal :: (MonadError String m, MonadState Store m) => LValue -> m (Maybe TypedVal)
getGlobal = getVar globalGetStoreFunc

setGlobal :: (MonadError String m, MonadState Store m) => LValue -> TypedVal -> m ()
setGlobal = setVar globalGetStoreFunc globalUpdateStoreFunc

----- Internal functions for setting/getting over an arbitrary VarStore -----
getVar :: (MonadError String m, MonadState Store m) => GetStoreFunc m -> LValue -> m (Maybe TypedVal)
getVar getS (LVar name) = do 
    store <- getS
    return (Map.lookup name store <|> 
            Map.lookup name store)
getVar getS (LArrInd arr' ind') = do
    arrM <- getVar getS arr'
    arr@(Typed arrV arrT) <- extractMaybeOrError arrM "Cannot resolve array"
    typeGuardArr arr "Cannot index into non-array type"
    ind@(Typed indV _) <- evalExp ind'
    typeGuard ind IntT "Index type must be an integer"
    case (arrV, indV, arrT) of
        (ArrayVal vals, IntVal i, ArrayT innerT) -> return $ Just (vals !! i `as` innerT)
        _ -> return Nothing

setVar :: (MonadError String m, MonadState Store m) => GetStoreFunc m -> UpdateStoreFunc m -> LValue -> TypedVal -> m ()
setVar getS updateS (LVar name) val = do
    store <- getS
    let updatedStore = Map.insert name val store in
        updateS updatedStore

setVar getS updateS (LArrInd arr' ind') (Typed val valT) = do
    arrM <- getVar getS arr'

    arr@(Typed arrV arrT) <- extractMaybeOrError arrM "Cannot resolve variable"
    ind@(Typed indV indT) <- evalExp ind'

    typeGuard ind IntT "Index type must be an integer"
    typeGuardArr arr "Cannot index into non-array type" 
    guardWithErrorMsg (ArrayT valT == arrT) "Inserting element of wrong type into array"

    case (arrV, indV) of
        (ArrayVal vals, IntVal i) -> 
            if i < 0 || length vals <= i then
                throwError "Array index out of bounds"
            else
                setVar getS updateS arr' (ArrayVal (setAtIndex vals i val) `as` arrT)
        _ -> error "ERR: Type system failure"
    where 
    -- | Credit to https://stackoverflow.com/questions/15530511/how-to-set-value-in-nth-element-in-a-haskell-list
    setAtIndex :: [a] -> Int -> a -> [a]
    setAtIndex l i v = Prelude.take i l ++ [v] ++ Prelude.drop (i + 1) l


----- Get/Set internal functions for KV store -----
getKV :: (MonadError String m, MonadState Store m) => String -> String -> m (Maybe TypedVal)
getKV rowKey colKey = do
    allVars <- get
    let res = do 
        row <- Map.lookup rowKey (persist allVars)
        Map.lookup colKey row
    return res

setKV :: (MonadError String m, MonadState Store m) => String -> String -> TypedVal -> m ()
setKV rowKey colKey val = do
    allVars <- get
    let row = Map.findWithDefault Map.empty rowKey (persist allVars)
    let updatedRow = Map.insert colKey val row
    let updatedKV = Map.insert rowKey updatedRow (persist allVars)
    let updatedStore = allVars {persist=updatedKV}
    put updatedStore


----- Error Handling and Type Checking -----
typeOf :: Value -> Maybe BType
typeOf (BoolVal _) = Just BoolT
typeOf (IntVal _) = Just IntT
typeOf (StringVal _) = Just StringT
typeOf (ArrayVal []) = Just $ ArrayT AnyT
typeOf (ArrayVal [h]) = typeOf h
typeOf (ArrayVal (h:t)) = do
    hType <- typeOf h
    tType <- typeOf (ArrayVal t)
    case tType of
        ArrayT inner -> if inner == hType then Just tType else Nothing
        _ -> Nothing

guardWithErrorMsg :: (MonadError String m, MonadState Store m) => Bool -> String -> m ()
guardWithErrorMsg b m = do if b then return () else throwError m

extractMaybeOrError :: (MonadError String m, MonadState Store m) => Maybe a -> String -> m a
extractMaybeOrError x msg = do
    case x of 
        Nothing -> throwError msg
        Just y -> return y

typeGuard :: (MonadError String m, MonadState Store m) => TypedVal -> BType -> String -> m ()
typeGuard _ AnyT m = do return ()
typeGuard (Typed _ t) t' m = if t == t' then return () else throwError m

typeGuardArr :: (MonadError String m, MonadState Store m) => TypedVal -> String -> m ()
typeGuardArr (Typed _ (ArrayT _)) _ = do return ()
typeGuardArr _ m = do throwError m


----- Expression evaluation helper functions -----
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

evalBinopIntExp :: (MonadError String m, MonadState Store m) => Exp -> String -> m TypedVal
evalBinopIntExp (BOp op e1 e2) opName = do
    e1'@(Typed v1 _) <- evalExp e1
    typeGuard e1' IntT ("Arguments for " <> opName <> " must be integers")
    e2'@(Typed v2 _) <- evalExp e2
    typeGuard e2' IntT ("Arguments for " <> opName <> " must be integers")
    case (v1, v2) of
        (IntVal v1', IntVal v2') -> return $ IntVal (intBinOpToF op v1' v2') `as` IntT 
        _ -> error "Typeguard doesn't work as expected"
evalBinopIntExp _ _ = error "Calling evalBinop without binop exp"

evalCompExp :: (MonadError String m, MonadState Store m) => Exp -> String -> m TypedVal
evalCompExp (BOp op e1 e2) opName = do
    e1'@(Typed v1 _) <- evalExp e1
    typeGuard e1' IntT ("Arguments for " <> opName <> " must be integers")
    e2'@(Typed v2 _) <- evalExp e2
    typeGuard e2' IntT ("Arguments for " <> opName <> " must be integers")
    case (v1, v2) of
        (IntVal v1', IntVal v2') -> return $ BoolVal (intCompToF op v1' v2') `as` BoolT 
        _ -> error "Typeguard doesn't work as expected"
evalCompExp _ _ = error "Calling evalCompExp without comp exp"

evalBoolOpExp :: (MonadError String m, MonadState Store m) => Exp -> String -> m TypedVal
evalBoolOpExp (BOp op e1 e2) opName = do
    e1'@(Typed v1 _) <- evalExp e1
    typeGuard e1' BoolT ("Arguments for " <> opName <> " must be booleans")
    e2'@(Typed v2 _) <- evalExp e2
    typeGuard e2' BoolT ("Arguments for " <> opName <> " must be booleans")
    case (v1, v2) of
        (BoolVal v1', BoolVal v2') -> return $ BoolVal (boolOpToF op v1' v2') `as` BoolT 
        _ -> error "Typeguard doesn't work as expected"
evalBoolOpExp _ _ = error "Calling evalCompExp without comp exp"


----- Main logic for expression evaluation -----
evalExp :: forall m. (MonadError String m, MonadState Store m) => Exp -> m TypedVal
evalExp (Val v) = do
    case typeOf v of
        Just t -> return $ v `as` t
        _ -> throwError $ "Type error" ++ show v
evalExp (Var s) = do
    var <- getLocal (LVar s)
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
            if i < 0 || length vals <= i then 
                throwError "Index out of bounds"
            else
                return (vals !! i `as` innerT)
        (ArrayT _, _, _, _) -> throwError "Index must be an integer"
        (_, _, IntT, _) -> throwError "Attempted index into non-array type"
        _ -> error "ERR: type system internal error"

evalExp (ArrCons []) = do return (ArrayVal [] `as` ArrayT AnyT)
evalExp (ArrCons [h]) = do
    Typed v t <- evalExp h
    return $ ArrayVal [v] `as` ArrayT t
evalExp (ArrCons (h:t)) = do
    Typed hv ht <- evalExp h
    Typed tv tt <- evalExp $ ArrCons t
    case (tv, tt) of
        (ArrayVal tvals, ArrayT inner) -> 
            if inner == ht then
                return $ ArrayVal (hv : tvals) `as` ArrayT inner
            else
                throwError "Type mismatch in array"
        _ -> error "ERR: Type system internal error"
    
evalExp (UOp Neg e) = do
    e1'@(Typed v1 _) <- evalExp e
    typeGuard e1' IntT "Argument for negation must be an integer"
    case v1 of
        IntVal v1' -> return $ IntVal (-v1') `as` IntT 
        _ -> error "Typeguard doesn't work as expected"
evalExp (UOp Not e) = do
    e1'@(Typed v1 _) <- evalExp e
    typeGuard e1' BoolT "Argument for not must be a boolean"
    case v1 of
        BoolVal v1' -> return $ BoolVal (not v1') `as` BoolT 
        _ -> error "Typeguard doesn't work as expected"

evalExp (FCall name args) = do
    allVars <- get
    f@(FDecl name argDecls expectedRetType body) <- extractMaybeOrError 
        (Map.lookup name (fdecls allVars)) 
        ("No function " ++ name)
    let prevLocals = locals allVars
    let newScopedStore = allVars {locals=Map.empty} 
    newScopedStore <- setParams (zip argDecls args)
    put allVars {locals=newScopedStore}
    ret <- evalBlock body Local
    put allVars {locals=prevLocals}
    case (ret, expectedRetType) of
        (Just x@(Typed retVal retTy), _) -> return x
        (Nothing, VoidT) -> return (IntVal 0 `as` VoidT)
        (_, _) -> throwError ("Return type for " <> name <> "doesn't match return value")

    where 
    setParams :: [(VarDecl, Exp)] -> m VarStore
    setParams [] = do return Map.empty
    setParams ((VDecl vty vname, exp) : t) = do
        e@(Typed eVal eTy) <- evalExp exp
        typeGuard e vty "Argument type doesn't match function declaration"
        rem <- setParams t
        return $ Map.insert vname e rem
        

evalStrExp :: String -> Doc
evalStrExp s =
    let res = doParse expP s in
    case res of
        Nothing -> PP.text "Parse error"
        Just (e, r) -> 
            let x = fst $ runIdentity (runStateT (runExceptT (evalExp e)) emptyStore) in
            case x of 
                Left err -> PP.text err
                Right val -> pp val

----- Statement evaluation -----
evalStatement :: (MonadError String m, MonadState Store m) => Statement -> Scope -> m (Maybe TypedVal)

evalStatement (Let v@(VDecl t name) exp) scope = do
    exists <- scopedGet scope (LVar name)
    guardWithErrorMsg (isNothing exists) "Error: redeclaring variable"
    (Typed ev et) <- evalExp exp
    guardWithErrorMsg (t == et) ("Error: incorrect type in declaration for " ++ name) 
    scopedSet scope (LVar name) (ev `as` et)
    return Nothing
evalStatement (Assign lval exp) scope = do
    exists <- scopedGet scope lval
    (Typed currentV expectedT) <- extractMaybeOrError exists "Error: assignment to undeclared variable"
    e <- evalExp exp
    typeGuard e expectedT "Error: Assignment must respect the use the same type"
    scopedSet scope lval e
    return Nothing
evalStatement (Return exp) scope = do
    tv <- evalExp exp
    return $ Just tv 
evalStatement (FCallStatement name args) scope = do
    e <- evalExp (FCall name args)
    typeGuard e VoidT "Calling function as statement with non-void return type"
    return Nothing
evalStatement (If exp b1 b2) scope = do
    e <- evalExp exp
    typeGuard e BoolT "If-else expression guard must be of type bool"
    case e of 
        Typed (BoolVal b) BoolT -> 
            if b then
                evalBlock b1 scope
            else 
                evalBlock b2 scope
        _ -> error "ERR: Type system internal error"
evalStatement (While exp body) scope = do
    e@(Typed expV expT) <- evalExp exp
    typeGuard e BoolT "Error: guard of while loop must be boolean type"
    case expV of
        BoolVal b -> 
            if b then do
                val <- evalBlock body scope
                case val of
                    Nothing -> evalStatement (While exp body) scope
                    Just x -> return $ Just x 
            else return Nothing  
        _ -> error "ERR: Type system internal error"

evalStrBlock :: String -> Doc
evalStrBlock s =
    let res = doParse blockP s in
    case res of
        Nothing -> PP.text "Parse error"
        Just (b, r) -> 
            let (x, store) = runIdentity (runStateT (runExceptT (evalBlock b Global)) emptyStore) in
            case x of 
                Left err -> PP.text err
                Right val -> PP.text "Success"

evalStrBlockStore :: String -> Store
evalStrBlockStore s =
    let res = doParse blockP s in
    case res of
        Nothing -> error "Parse error"
        Just (b, r) -> 
            let (x, store) = runIdentity (runStateT (runExceptT (evalBlock b Global)) emptyStore) in
            case x of 
                Left err -> error err
                Right val -> store
            
evalBlock :: (MonadError String m, MonadState Store m) => Block -> Scope -> m (Maybe TypedVal)
evalBlock (Block []) scope = do return Nothing
evalBlock (Block (h:t)) scope = do
    res <- evalStatement h scope
    case res of
        Nothing -> evalBlock (Block t) scope
        _ -> return res

evalQuery :: (MonadError String m, MonadState Store m) => Query -> m (Maybe TypedVal)
evalQuery (Query fdecls main) = do
    let ftable = Map.fromAscList (map (\f@(FDecl name _ _ _)-> (name, f)) fdecls)
        initStore = emptyStore {fdecls=ftable} in
        do 
            put initStore
            evalBlock main Global

----- Helper function for testing -----
evalQueryStr :: String -> Doc
evalQueryStr s =
    let res = doParse queryP s in
    case res of
        Nothing -> PP.text "Parse error"
        Just (q, r) -> 
            let (x, store) = runIdentity (runStateT (runExceptT (evalQuery q)) emptyStore) in
            case x of 
                Left err -> PP.text err
                Right Nothing -> PP.text "Success: void"
                Right (Just result) -> PP.text "Success : " <> PP.parens (pp result)