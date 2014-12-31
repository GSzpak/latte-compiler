module Frontend where


import AbsLatte
import PrintLatte
import qualified Data.Map as Map
import Control.Monad.Error
import Control.Monad.Reader
import Text.Printf


class Typable a where
    getType :: a -> Type


type BlockDepth = Integer
data VarEnvElem = VarEnvElem {type_ :: Type, blockDepth :: BlockDepth}
type FunEnvElem = Type

instance Typable Type where
    getType t = t

instance Typable VarEnvElem where
    getType varEnvElem = type_ varEnvElem

instance Typable Arg where
    getType (Arg type_ _) = type_

type TypeEnv a = Map.Map Ident a
type VarEnv = TypeEnv VarEnvElem
type FunEnv = TypeEnv FunEnvElem
data Env = Env {
    varEnv :: VarEnv,
    funEnv :: FunEnv,
    actBlockDepth :: BlockDepth,
    actReturnType :: Type
}
type TypeEval a = ReaderT Env (ErrorT String IO) a


runTypeEval :: Env -> TypeEval a -> IO (Either String a)
runTypeEval env eval = runErrorT (runReaderT eval env)

emptyEnv :: Env
emptyEnv = Env {
    varEnv = Map.empty,
    funEnv = Map.empty,
    actBlockDepth = 0,
    actReturnType = Void
}

name :: Ident -> String
name (Ident s) = s

incomaptibleTypesErr :: Type -> Type -> String
incomaptibleTypesErr expected actual =
    printf "Incompatible types: expected %s, got %s" (printTree expected) (printTree actual)

unexpectedTypeErr :: Type -> String
unexpectedTypeErr t = printf "Unexpected type: %s" (printTree t)

incompatibleParametersErr :: Ident -> [Type] -> [Type] -> String
incompatibleParametersErr ident formal act = 
    printf "Incompatible parameters for function %s: expected %s, got %s" (name ident) printFormal printAct
    where
        printFormal = show $ map printTree formal
        printAct = show $ map printTree act

undeclaredFunctionErr :: Ident -> String
undeclaredFunctionErr ident = printf "Undeclared function: %s" (name ident)

undeclaredVariableErr :: Ident -> String
undeclaredVariableErr ident = printf "Undeclared variable: %s" (name ident)

duplicatedVariableErr :: Ident -> String
duplicatedVariableErr ident = printf "Duplicated variable declaration: %s" (name ident)

duplicatedFunErr :: Ident -> String
duplicatedFunErr ident = printf "Duplicated function declaration: %s" (name ident)

getIdentType :: Typable a => Ident -> (Env -> TypeEnv a) -> String -> TypeEval Type
getIdentType ident envSelector errMessage = do
    env <- ask
    case Map.lookup ident (envSelector env) of
        Just envElem -> return $ getType envElem
        Nothing -> throwError errMessage

checkTypes :: Type -> Type -> TypeEval Type
checkTypes actType expectedType = do
    if actType /= expectedType then
        throwError $ incomaptibleTypesErr expectedType actType
    else
        return actType

checkExprType :: Expr -> Type -> TypeEval Type
checkExprType expr expectedType = do
    exprType <- evalExprType expr
    checkTypes exprType expectedType

checkTwoArgExpression :: Expr -> Expr -> [Type] -> TypeEval Type
checkTwoArgExpression expr1 expr2 expectedTypes = do
    t1 <- evalExprType expr1
    t2 <- evalExprType expr2
    checkTypes t1 t2
    -- t1 == t2
    let resultType = t1
    if resultType `elem` expectedTypes then
        return resultType
    else
        throwError $ unexpectedTypeErr resultType

evalExprType :: Expr -> TypeEval Type
evalExprType (EVar ident) = getIdentType ident varEnv (undeclaredVariableErr ident)
evalExprType (ELitInt _) = return Int
evalExprType ELitTrue = return Bool
evalExprType ELitFalse = return Bool
evalExprType (EString _) = return Str
evalExprType (Neg expr) = checkExprType expr Int
evalExprType (Not expr) = checkExprType expr Bool
evalExprType (EMul expr1 _ expr2) = checkTwoArgExpression expr1 expr2 [Int]
evalExprType (EAdd expr1 Plus expr2) = checkTwoArgExpression expr1 expr2 [Int, Str]
evalExprType (EAdd expr1 Minus expr2) = checkTwoArgExpression expr1 expr2 [Int]
evalExprType (ERel expr1 _ expr2) = checkTwoArgExpression expr1 expr2 [Int, Bool, Str]
evalExprType (EAnd expr1 expr2) = checkTwoArgExpression expr1 expr2 [Bool]
evalExprType (EOr expr1 expr2) = checkTwoArgExpression expr1 expr2 [Bool]
evalExprType (EApp ident arguments) = do
    Fun type_ argTypes <- getIdentType ident funEnv (undeclaredFunctionErr ident)
    actTypes <- mapM evalExprType arguments
    if argTypes == actTypes then
        return type_
    else
        throwError $ incompatibleParametersErr ident argTypes actTypes 

checkIfVarDeclared :: Ident -> TypeEval ()
checkIfVarDeclared ident = do
    env <- ask
    case Map.lookup ident (varEnv env) of
        Just (VarEnvElem t depth) ->
            (if depth <= (actBlockDepth env) then
                throwError $ duplicatedVariableErr ident
            else
                return ())
        Nothing -> return ()

declareVar :: Ident -> Type -> Env -> Env
declareVar ident t env = 
    let
        newVar = VarEnvElem {
            type_ = t,
            blockDepth = actBlockDepth env
        }
    in 
        Env {
            varEnv = Map.insert ident newVar (varEnv env),
            funEnv = funEnv env,
            actBlockDepth = actBlockDepth env,
            actReturnType = actReturnType env
        }

checkStmt :: Stmt -> TypeEval Env
checkStmt Empty = ask
checkStmt (BStmt block) =
    local updateBlockDepth (checkBlock block)
    where
        updateBlockDepth :: Env -> Env
        updateBlockDepth env = Env {
            varEnv = varEnv env,
            funEnv = funEnv env,
            actBlockDepth = (actBlockDepth env) + 1,
            actReturnType = actReturnType env
        }
checkStmt (Decl type_ items) = checkDeclaration items
    where
        checkDeclaration :: [Item] -> TypeEval Env
        checkDeclaration [] = ask
        checkDeclaration ((NoInit ident):items) = do
            checkIfVarDeclared ident
            local (declareVar ident type_) (checkDeclaration items)
        checkDeclaration ((Init ident expr):items) = do
            checkIfVarDeclared ident
            checkExprType expr type_
            local (declareVar ident type_) (checkDeclaration items)
checkStmt (Ass ident expr) = do 
    identType <- getIdentType ident varEnv (undeclaredVariableErr ident)
    exprType <- evalExprType expr
    checkTypes identType exprType
    ask
checkStmt (Incr ident) = do
    identType <- getIdentType ident varEnv (undeclaredVariableErr ident)
    checkTypes identType Int
    ask
checkStmt (Decr ident) = do
    identType <- getIdentType ident varEnv (undeclaredVariableErr ident)
    checkTypes identType Int
    ask
checkStmt (Ret expr) = do
    env <- ask
    if actReturnType env == Void then
        throwError $ printf "Returning expression: %s in a void function" (printTree expr)
    else do
        checkExprType expr (actReturnType env)
        ask
checkStmt VRet = do
    env <- ask
    checkTypes Void (actReturnType env)
    return env
checkStmt (Cond expr stmt) = do
    checkExprType expr Bool
    checkStmt stmt
checkStmt (CondElse expr ifStmt elseStmt) = do
    checkExprType expr Bool
    checkStmt ifStmt
    checkStmt elseStmt
checkStmt (While expr stmt) = do
    checkExprType expr Bool
    checkStmt stmt

checkStatements :: [Stmt] -> TypeEval Env
checkStatements [] = ask
checkStatements (stmt:statements) = do
    env <- checkStmt stmt
    local (\_ -> env) (checkStatements statements)

checkBlock :: Block -> TypeEval Env
checkBlock (Block statements) = checkStatements statements

declareArgs :: [Arg] -> TypeEval Env
declareArgs [] = ask
declareArgs ((Arg type_ ident):args) = do
    checkIfVarDeclared ident
    local (declareVar ident type_) (declareArgs args)

{-
evalConstExpr :: Expr -> TypeEval ExprVal

checkRet (Block stmts) = do
    let last = tail stmts
    case last of 
        VRet -> return True
        Ret _ -> return True
        Bstmt block -> checkRet block
        if -> do
            val <- evalConstExpr
            if val == True then checkStmtRet stmt else throwError
        if else
            val <- evalConstExpr `catchError` (\_ -> do checkRetStmt stmt1, stmt2)
            case val of
                True -> checkStmtRet stmt1
                False -> 2
                _ -> check 1, 2

-}
checkFunction :: TopDef -> TypeEval Env
checkFunction (FnDef type_ _ args block) = do
    env <- declareArgs args
    let env' = prepareBlockCheck env
    local (\_ -> env') (checkBlock block)
    where
        prepareBlockCheck :: Env -> Env
        prepareBlockCheck env = Env {
            varEnv = varEnv env,
            funEnv = funEnv env,
            actBlockDepth = (actBlockDepth env) + 1,
            actReturnType = type_
        }

checkFunctions :: [TopDef] -> TypeEval ()
checkFunctions topDefinitions = sequence_ $ map checkFunction topDefinitions

checkIfFunDeclared :: Ident -> TypeEval ()
checkIfFunDeclared ident = do
    env <- ask
    case Map.lookup ident (funEnv env) of
        Just _ -> throwError $ duplicatedFunErr ident
        Nothing -> return ()

declareFunctions :: [TopDef] -> TypeEval Env
declareFunctions [] = ask
declareFunctions ((FnDef type_ ident args _):defs) = do
    env <- ask
    checkIfFunDeclared ident
    local declareFun (declareFunctions defs)
    where
        argTypes = map getType args
        declareFun :: Env -> Env
        declareFun env = Env {
            varEnv = varEnv env,
            funEnv = Map.insert ident (Fun type_ argTypes) (funEnv env),
            actBlockDepth = actBlockDepth env,
            actReturnType = actReturnType env
        }

-- TODO: add declared

checkProgram :: Program -> TypeEval ()
checkProgram (Program topDefinitions) = do
    env <- declareFunctions topDefinitions
    local (\_ -> env) (checkFunctions topDefinitions)
