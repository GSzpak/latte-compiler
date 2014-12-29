module Frontend where

import AbsLatte
import PrintLatte
import qualified Data.Map as Map
import Control.Monad.Error
import Control.Monad.Reader


type BlockDepth = Integer
data VarEnvElem = VarEnvElem {type_ :: Type, blockDepth :: BlockDepth}
type FunEnvElem = Type
type VarEnv = Map.Map Ident VarEnvElem
type FunEnv = Map.Map Ident FunEnvElem
data Env = Env {
    varEnv :: VarEnv,
    funEnv :: FunEnv,
    actBlockDepth :: BlockDepth,
    actReturnType :: Type
}
type TypeEval a = ReaderT Env (ErrorT String IO) a


runTypeEval :: Env -> TypeEval a -> IO (Either String IO) a
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

getType :: Arg -> Type
getType (Arg type_ _) = type_

incomaptibleTypesErr :: Type -> Type -> String
incomaptibleTypesErr expected actual =
    "Incompatible types: expected " ++ (printTree expected) ++ ", got " ++ (printTree actual)

unexpectedTypeErr :: Type -> String
unexpectedTypeErr t = "Unexpected type: " ++ (printTree t)

checkExprType :: Expr -> Type -> TypeEval Type
checkExprType expr expectedType = do
    exprType <- evalExprType expr
    checkTypes exprType expectedType

checkTypes :: Type -> Type -> TypeEval Type
checkTypes actType expectedType = do
    if actType /= expectedType then
        throwError $ incomaptibleTypesErr expectedType actType
    else
        return actType

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

getIdentType :: Ident -> TypeEval Type
getIdentType ident = do
    env <- ask
    case Map.lookup ident (varEnv env) of
        Just t -> return t
        Nothing -> throwError $ "Undeclared variable: " ++ (name ident)

evalExprType :: Expr -> TypeEval Type
evalExprType (EVar ident) = getIdentType ident
evalExprType (ElitInt _) = return Int
evalExprType ElitTrue = return Bool
evalExprType ELitFalse = return Bool
evalExprType (EString _) = return Str
evalExprType (Neg expr) = checkExprType expr Int
evalExprType (Not expr) = checkExprType expr Bool
evalExprType (EMul expr1 _ expr2) = checkTwoArgExpression expr1 expr2 [Int]
evalExprType (EAdd expr1 Plus expr2) = checkTwoArgExpression expr1 expr2 [Int, Str]
evalExprType (EAdd expr1 Minus expr2) = checkTwoArgExpression expr1 expr2 [Int]
evalExprType (ERel expr1 _ expr2) = checkTwoArgExpression expr1 expr2 [Int, Bool, Str]
evalExprType (EAnd expr1 _ expr2) = checkTwoArgExpression expr1 expr2 [Bool]
evalExprType (EOr expr1 _ expr2) = checkTwoArgExpression expr1 expr2 [Bool]
-- TODO: EApp

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
-- TODO: Decl
checkStmt (Ass ident expr) = do 
    identType <- getIdentType ident
    exprType <- evalExprType expr
    checkTypes identType exprType
    ask
checkStmt (Incr ident) = do
    identType <- getIdentType
    checkTypes identType Int
    ask
checkStmt (Decr ident) = do
    identType <- getIdentType
    checkTypes identType Int
    ask
-- TODO: Ret
-- TODO: VRet
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

checkStatements :: [Stmt] -> Type -> TypeEval Env
checkStatements [] _ = ask
checkStatements (stmt:statements) returnType = do
    env <- checkStmt stmt returnType
    local (\_ -> env) (checkStatements statements)

checkBlock :: Block -> TypeEval Env
checkBlock (Block statements) = checkStatements statements

checkFunction :: TopDef -> TypeEval Env
checkFunction (FnDef type_ _ args block) = do
    -- TODO: decl args
    local updateReturnType (checkBlock block)
    where
        updateReturnType :: Env -> Env
        updateReturnType env = Env {
            varEnv = varEnv env,
            funEnv = funEnv env,
            actBlockDepth = actBlockDepth env,
            actReturnType = type_
        }

checkFunctions :: [TopDef] -> TypeEval ()
checkFunctions topDefinitions = sequence_ $ map checkFunction topDefinitions

declareFunctions :: [TopDef] -> TypeEval Env
declareFunctions [] = ask
declareFunctions ((FnDef type_ ident args _):defs) = 
    local declareFun (declareFunctions defs)
    where
        argTypes = map getType args
        declareFun :: Env -> Env
        declareFun env = Env {
            varEnv = varEnv env,
            funEnv = Map.insert ident (Fun type_ argTypes) (funEnv env)
            actBlockDepth = actBlockDepth env
        }

checkProgram :: Program -> TypeEval ()
checkProgram (Program topDefinitions) = do
    env <- declareFunctions topDefinitions
    local (\_ -> env) (checkFunctions topDefinitions)
