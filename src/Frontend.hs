module Frontend where


import AbsLatte
import PrintLatte
import Data.List as List
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

instance Typable TopDef where
    getType (FnDef type_ _ args _) = Fun type_ (map getType args)

type TypeEnv a = Map.Map Ident a
type VarEnv = TypeEnv VarEnvElem
type FunEnv = TypeEnv FunEnvElem
data Env = Env {
    varEnv :: VarEnv,
    funEnv :: FunEnv,
    actBlockDepth :: BlockDepth,
    actReturnType :: Type
}

type Eval a = ReaderT Env (ErrorT String IO) a


runEval :: Env -> Eval a -> IO (Either String a)
runEval env eval = runErrorT (runReaderT eval env)

emptyEnv :: Env
emptyEnv = Env {
    varEnv = Map.empty,
    funEnv = Map.empty,
    actBlockDepth = 0,
    actReturnType = Void
}

name :: Ident -> String
name (Ident s) = s

incomaptibleTypesErr :: Print a => Type -> Type -> a -> String
incomaptibleTypesErr expected actual printable =
    printf "Incompatible types in %s: expected %s, got %s" (printTree printable) (printTree expected) (printTree actual)

unexpectedTypeErr :: Type -> Expr -> String
unexpectedTypeErr t expr = printf "Unexpected type: %s in expression: %s" (printTree t) (printTree expr)

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

getIdentType :: Typable a => Ident -> (Env -> TypeEnv a) -> String -> Eval Type
getIdentType ident envSelector errMessage = do
    env <- ask
    case Map.lookup ident (envSelector env) of
        Just envElem -> return $ getType envElem
        Nothing -> throwError errMessage

checkTypes :: Print a => Type -> Type -> a -> Eval Type
checkTypes actType expectedType printable = do
    if actType /= expectedType then
        throwError $ incomaptibleTypesErr expectedType actType printable
    else
        return actType

checkExprType :: Expr -> Type -> Eval Type
checkExprType expr expectedType = do
    exprType <- evalExprType expr
    checkTypes exprType expectedType expr

checkTwoArgExpression :: Expr -> Expr -> [Type] -> Expr -> Eval Type
checkTwoArgExpression expr1 expr2 expectedTypes printable = do
    t1 <- evalExprType expr1
    t2 <- evalExprType expr2
    checkTypes t1 t2 printable
    -- t1 == t2
    let resultType = t1
    if resultType `elem` expectedTypes then
        return resultType
    else
        throwError $ unexpectedTypeErr resultType printable

evalExprType :: Expr -> Eval Type
evalExprType (EVar ident) = getIdentType ident varEnv (undeclaredVariableErr ident)
evalExprType (ELitInt _) = return Int
evalExprType ELitTrue = return Bool
evalExprType ELitFalse = return Bool
evalExprType (EString _) = return Str
evalExprType (Neg expr) = checkExprType expr Int
evalExprType (Not expr) = checkExprType expr Bool
evalExprType e@(EMul expr1 _ expr2) = checkTwoArgExpression expr1 expr2 [Int] e
evalExprType e@(EAdd expr1 Plus expr2) = checkTwoArgExpression expr1 expr2 [Int, Str] e
evalExprType e@(EAdd expr1 Minus expr2) = checkTwoArgExpression expr1 expr2 [Int] e
evalExprType e@(ERel expr1 _ expr2) = checkTwoArgExpression expr1 expr2 [Int, Bool, Str] e
evalExprType e@(EAnd expr1 expr2) = checkTwoArgExpression expr1 expr2 [Bool] e
evalExprType e@(EOr expr1 expr2) = checkTwoArgExpression expr1 expr2 [Bool] e
evalExprType e@(EApp ident arguments) = do
    Fun type_ argTypes <- getIdentType ident funEnv (undeclaredFunctionErr ident)
    actTypes <- mapM evalExprType arguments
    if argTypes == actTypes then
        return type_
    else
        throwError $ incompatibleParametersErr ident argTypes actTypes 

checkIfVarDeclared :: Ident -> Eval ()
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

checkStmt :: Stmt -> Eval Env
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
        checkDeclaration :: [Item] -> Eval Env
        checkDeclaration [] = ask
        checkDeclaration ((NoInit ident):items) = do
            checkIfVarDeclared ident
            local (declareVar ident type_) (checkDeclaration items)
        checkDeclaration ((Init ident expr):items) = do
            checkIfVarDeclared ident
            checkExprType expr type_
            local (declareVar ident type_) (checkDeclaration items)
checkStmt s@(Ass ident expr) = do 
    identType <- getIdentType ident varEnv (undeclaredVariableErr ident)
    exprType <- evalExprType expr
    checkTypes identType exprType s
    ask
checkStmt s@(Incr ident) = do
    identType <- getIdentType ident varEnv (undeclaredVariableErr ident)
    checkTypes identType Int s
    ask
checkStmt s@(Decr ident) = do
    identType <- getIdentType ident varEnv (undeclaredVariableErr ident)
    checkTypes identType Int s
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
    checkTypes Void (actReturnType env) VRet
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
checkStmt (SExp expr) = do
    evalExprType expr
    ask

checkStatements :: [Stmt] -> Eval Env
checkStatements [] = ask
checkStatements (stmt:statements) = do
    env <- checkStmt stmt
    local (\_ -> env) (checkStatements statements)

checkBlock :: Block -> Eval Env
checkBlock (Block statements) = checkStatements statements

-- Expression evaluation is run after type checking.
-- Throws error if evaluation is not possible during compilation.

{-

data ExprConst = IntConst Integer | BoolConst Bool | StrConst String

instance Eq ExprConst where
    (IntConst n1) == (IntConst n2) = (n1 == n2)
    (BoolConst b1) == (BoolConst b2) = (b1 == b2)
    (StrConst s1) == (StrConst s2) = (s1 == s2)
    _ == _ = False

instance Ord ExprConst where
    (IntConst n1) <= (IntConst n2) = (n1 <= n2)
    (BoolConst b1) <= (BoolConst b2) = (b1 <= b2)
    (StrConst s1) <= (StrConst s2) = (s1 <= s2)

getExpFromConst :: ExprConst -> Expr
getExpFromConst (IntConst n) = ELitInt n
getExpFromConst (BoolConst True) = ELitTrue
getExpFromConst (BoolConst False) = ELitFalse
getExpFromConst (StrConst s) = EString s

evaluationNotPossible :: String
evaluationNotPossible = "Not possible"

evaluationError :: String
evaluationError = "Error while evaluating constant expression"

evalConstExpr :: Expr -> Eval ExprConst
evalConstExpr (EVar ident) = throwError evaluationNotPossible
evalConstExpr (ELitInt n) = return (IntConst n)
evalConstExpr ELitTrue = return (BoolConst True)
evalConstExpr ELitFalse = return (BoolConst False)
evalConstExpr (EString s) = return (StrConst s)
evalConstExpr (Not expr) = do
    BoolConst b <- evalConstExpr expr
    return $ BoolConst $ not b
evalConstExpr (Neg expr) = do
    IntConst n <- evalConstExpr expr
    return $ IntConst $ -n
evalConstExpr (EMul e1 Times e2) = evalConstIntExpr e1 e2 (*) False
evalConstExpr (EMul e1 Div e2) = evalConstIntExpr e1 e2 div True
evalConstExpr (EMul e1 Mod e2) = evalConstIntExpr e1 e2 mod True
evalConstExpr (EAdd e1 Plus e2) = do
    exprType <- evalExprType e1
    case exprType of 
        Int -> evalConstIntExpr e1 e2 (+) False
        Str -> do
            StrConst s1 <- evalConstExpr e1
            StrConst s2 <- evalConstExpr e2
            return $ StrConst $ s1 ++ s2
evalConstExpr (EAdd e1 Minus e2) = evalConstIntExpr e1 e2 (-) False
evalConstExpr (EAnd e1 e2) = evalConstBoolExpr e1 e2 (&&)
evalConstExpr (EAnd e1 e2) = evalConstBoolExpr e1 e2 (||)
evalConstExpr (ERel e1 LTH e2) = evalCompareExpr e1 e2 (<)
evalConstExpr (ERel e1 LE e2) = evalCompareExpr e1 e2 (<=)
evalConstExpr (ERel e1 GTH e2) = evalCompareExpr e1 e2 (>)
evalConstExpr (ERel e1 GE e2) = evalCompareExpr e1 e2 (>=)
evalConstExpr (ERel e1 EQU e2) = evalCompareExpr e1 e2 (==)
evalConstExpr (ERel e1 NE e2) = evalCompareExpr e1 e2 (/=)
evalConstExpr (EApp _ exprs) 

foldConstIntExpr :: Expr -> Expr -> (Integer -> Integer -> Integer) -> Bool -> Eval ExprConst
foldConstIntExpr e1 e2 oper checkDivision = do
    IntConst n1 <- evalConstExpr e1
    IntConst n2 <- evalConstExpr e2
    if checkDivision && n2 == 0 then
        throwError evaluationError
    else
        return $ IntConst $ oper n1 n2

evalConstBoolExpr :: Expr -> Expr -> (Bool -> Bool -> Bool) -> Eval ExprConst
evalConstBoolExpr e1 e2 oper = do
    BoolConst b1 <- evalConstExpr e1
    BoolConst b2 <- evalConstExpr e2
    return $ BoolConst $ oper b1 b2

evalCompareExpr :: Expr -> Expr -> (ExprConst -> ExprConst -> Bool) -> Eval ExprConst
evalCompareExpr e1 e2 oper = do
    val1 <- evalConstExpr e1
    val2 <- evalConstExpr e2
    return $ BoolConst $ oper val1 val2
-}

isConstant :: Expr -> Bool
isConstant (ELitInt _) = True
isConstant ELitTrue = True
isConstant ELitFalse = True
isConstant (EString s) = True
isConstant expr = False

exprFromBool :: Bool -> Expr
exprFromBool True = ELitTrue
exprFromBool False = ELitFalse

boolFromExpr :: Expr -> Bool
boolFromExpr ELitTrue = True
boolFromExpr ELitFalse = False

foldBoolExpr :: Expr -> Expr -> (Expr -> Expr -> Expr) -> (Bool -> Bool -> Bool) -> Eval Expr
foldBoolExpr expr1 expr2 constructor fun = do
    folded1 <- foldConstExpr expr1
    folded2 <- foldConstExpr expr2
    if (isConstant folded1 && isConstant folded2) then
        return $ exprFromBool $ fun (boolFromExpr folded1) (boolFromExpr folded2)
    else
        return $ constructor folded1 folded2

foldBinOpExpr :: Expr -> Expr -> (Expr -> Expr -> Expr) -> (Integer -> Integer -> Integer) -> Bool -> Eval Expr
foldBinOpExpr e1 e2 constructor fun checkDivision = do
    folded1 <- foldConstExpr e1
    folded2 <- foldConstExpr e2
    case (folded1, folded2) of
        (ELitInt n1, ELitInt n2) ->
            (if checkDivision && n2 == 0 then
                throwError "Division by zero"
            else
                return $ ELitInt $ fun n1 n2)
        _ -> return $ constructor folded1 folded2

foldConstExpr :: Expr -> Eval Expr
foldConstExpr (EVar ident) = return $ EVar ident
foldConstExpr (ELitInt n) = return $ ELitInt n
foldConstExpr ELitTrue = return ELitTrue
foldConstExpr ELitFalse = return ELitFalse
foldConstExpr (EString s) = return $ EString s
foldConstExpr (Not expr) = do
    folded <- foldConstExpr expr
    case folded of
        ELitTrue -> return ELitFalse
        ELitFalse -> return ELitTrue
        expr -> return (Not expr)
foldConstExpr (Neg expr) = do
    folded <- foldConstExpr expr
    case folded of
        ELitInt n -> return $ ELitInt (-n)
        expr -> return (Neg expr)
foldConstExpr (EApp ident exprs) = do
    foldedArgs <- mapM foldConstExpr exprs
    return $ EApp ident foldedArgs
foldConstExpr (EMul e1 Times e2) = foldBinOpExpr e1 e2 ((flip EMul) Times) (*) False 
foldConstExpr (EMul e1 Div e2) = foldBinOpExpr e1 e2 ((flip EMul) Div) div True
foldConstExpr (EMul e1 Mod e2) = foldBinOpExpr e1 e2 ((flip EMul) Mod) mod True
foldConstExpr (EAdd e1 Plus e2) = do
    folded1 <- foldConstExpr e1
    folded2 <- foldConstExpr e2
    case (folded1, folded2) of 
        (EString s1, EString s2) -> return $ EString $ s1 ++ s2
        (ELitInt n1, ELitInt n2) -> return $ ELitInt $ n1 + n2
        _ -> return $ EAdd folded1 Plus folded2
foldConstExpr (EAdd e1 Minus e2) = foldBinOpExpr e1 e2 ((flip EAdd) Minus) (-) False
foldConstExpr (EAnd e1 e2) = foldBoolExpr e1 e2 EAnd (&&)
foldConstExpr (EOr e1 e2) = foldBoolExpr e1 e2 EOr (||)
foldConstExpr (ERel e1 LTH e2) = foldBoolExpr e1 e2 ((flip ERel) LTH) (<)
foldConstExpr (ERel e1 LE e2) = foldBoolExpr e1 e2 ((flip ERel) LE) (<=)
foldConstExpr (ERel e1 GTH e2) = foldBoolExpr e1 e2 ((flip ERel) GTH) (>)
foldConstExpr (ERel e1 GE e2) = foldBoolExpr e1 e2 ((flip ERel) GE) (>=)
foldConstExpr (ERel e1 EQU e2) = foldBoolExpr e1 e2 ((flip ERel) EQU) (==)
foldConstExpr (ERel e1 NE e2) = foldBoolExpr e1 e2 ((flip ERel) NE) (/=)

foldConstants :: Stmt -> Eval Stmt
foldConstants (BStmt block) = do
    folded <- foldConstantsInBlock block
    return $ BStmt folded
foldConstants (Ass ident expr) = do 
    folded <- foldConstExpr expr
    return $ Ass ident folded
foldConstants (Ret expr) = do
    folded <- foldConstExpr expr
    return $ Ret folded
foldConstants (Cond expr stmt) = do
    folded <- foldConstExpr expr
    return $ Cond folded stmt
foldConstants (CondElse expr stmt1 stmt2) = do
    folded <- foldConstExpr expr
    return $ CondElse folded stmt1 stmt2
foldConstants (While expr stmt) = do
    folded <- foldConstExpr expr
    return $ While folded stmt
foldConstants (SExp expr) = do
    folded <- foldConstExpr expr
    return $ SExp expr
foldConstants stmt = return stmt

foldConstantsInBlock :: Block -> Eval Block
foldConstantsInBlock (Block stmts) = do
    folded <- mapM foldConstants stmts
    return $ Block folded

-- Deletes unreachable statements
optimizeStmt :: Stmt -> Stmt
optimizeStmt (BStmt block) = BStmt $ optimizeBlock block
optimizeStmt (Cond expr stmt) = case expr of
    ELitTrue -> optimizeStmt stmt
    ELitFalse -> Empty
    _ -> Cond expr (optimizeStmt stmt)
optimizeStmt (CondElse expr stmt1 stmt2) = case expr of
    ELitTrue -> optimizeStmt stmt1
    ELitFalse -> optimizeStmt stmt2
    _ -> CondElse expr (optimizeStmt stmt1) (optimizeStmt stmt2)
optimizeStmt (While expr stmt) = case expr of
    ELitFalse -> Empty
    ELitTrue -> 
        (if hasReturn optimizedBody then
            optimizedBody
        else
            While expr optimizedBody)
    _ -> While expr optimizedBody
    where
        optimizedBody = optimizeStmt stmt
optimizeStmt stmt = stmt

optimizeBlock :: Block -> Block
optimizeBlock (Block stmts) = do
    let
        optimized = filter (/= Empty) (map optimizeStmt stmts)
    case List.findIndex hasReturn optimized of
        Nothing -> Block optimized
        Just index -> Block $ take (index + 1) optimized

-- Checked after deleting unreachable code
hasReturn :: Stmt -> Bool
hasReturn (Ret _) = True
hasReturn VRet = True
hasReturn (CondElse _ stmt1 stmt2) = (hasReturn stmt1) && (hasReturn stmt2)
hasReturn _ = False

-- After deleting unnecessary statements
{-
checkStmtForReturn :: Stmt -> Bool
checkStmtForReturn (Ret _ ) = True
checkStmtForReturn VRet = True
checkStmtForReturn (BStmt block) = checkBlockForReturn block
checkStmtForReturn (Cond expr stmt) =
    case expr of
        ELitTrue -> checkStmtForReturn stmt
        _ -> False
checkStmtForReturn (While expr stmt) = checkStmtForReturn (Cond expr stmt)
checkStmtForReturn (CondElse expr stmt1 stmt2) =
    case expr of
        ELitTrue -> checkStmtForReturn stmt1
        ELitFalse -> checkStmtForReturn stmt2
        _ -> (checkStmtForReturn stmt1) && (checkStmtForReturn stmt2)
checkStmtForReturn _ = False

checkBlockForReturn :: Block -> Bool
checkBlockForReturn (Block stmts) = any checkStmtForReturn stmts

checkBlockForUnreachableCode :: Block -> Eval Bool
checkBlockForUnreachableCode (Block []) = return False
checkBlockForUnreachableCode (Block (stmt:stmts)) = do
    isUnreachable <- checkStmtForReturn stmt
    if isUnreachable then
        return True
    else
        checkBlockForUnreachableCode (Block stmts)

checkFunForReturn :: TopDef -> Eval ()
checkFunForReturn (FnDef _ ident _ block) = do
    isReturn <- checkBlockForReturn block
    if not isReturn then
        throwError $ printf "No \"return\" instruction in function %s" (name ident)
    else
        return ()

checkFunForUnreachableCode :: TopDef -> Eval ()
checkFunForUnreachableCode (FnDef _ ident _ block) = do
    isUnreachable <- checkBlockForUnreachableCode block
    if isUnreachable then
        throwError $ printf "Unreachable code in function %s" (name ident)
    else
        return ()
-}

checkFunction :: TopDef -> Eval TopDef
checkFunction fun@(FnDef type_ ident args block) = do
    env <- declareArgs args
    let env' = prepareBlockCheck env
    local (\_ -> env') (checkBlock block)
    foldedConstants <- foldConstantsInBlock block
    let (Block optimized) = optimizeBlock foldedConstants
    if (type_ /= Void) && (not (any hasReturn optimized)) then
        throwError $ printf "No \"return\" instruction in function %s" (name ident)
    else
        return $ FnDef type_ ident args (Block optimized)
    where
        prepareBlockCheck :: Env -> Env
        prepareBlockCheck env = Env {
            varEnv = varEnv env,
            funEnv = funEnv env,
            actBlockDepth = (actBlockDepth env) + 1,
            actReturnType = type_
        }
        declareArgs :: [Arg] -> Eval Env
        declareArgs [] = ask
        declareArgs ((Arg type_ ident):args) = do
            checkIfVarDeclared ident
            local (declareVar ident type_) (declareArgs args)

checkIfFunDeclared :: Ident -> Eval ()
checkIfFunDeclared ident = do
    env <- ask
    case Map.lookup ident (funEnv env) of
        Just _ -> throwError $ duplicatedFunErr ident
        Nothing -> return ()

declareFunctions :: [TopDef] -> Eval Env
declareFunctions [] = ask
declareFunctions (fun@(FnDef type_ ident args _):defs) = do
    env <- ask
    checkIfFunDeclared ident
    local declareFun (declareFunctions defs)
    where
        argTypes = map getType args
        declareFun :: Env -> Env
        declareFun env = Env {
            varEnv = varEnv env,
            funEnv = Map.insert ident (getType fun) (funEnv env),
            actBlockDepth = actBlockDepth env,
            actReturnType = actReturnType env
        }

declareBuiltIn :: Eval Env
declareBuiltIn =
    let
        builtIn = 
            [(Ident "printInt", Fun Void [Int]),
            (Ident "printString", Fun Void [Str]),
            (Ident "error", Fun Void []),
            (Ident "readInt", Fun Int []),
            (Ident "readString", Fun Str [])]
    in 
        local (addFunList builtIn) ask
    where
        addFunList :: [(Ident, Type)] -> Env -> Env
        addFunList funList env = Env {
            varEnv = varEnv env,
            funEnv = Map.union (funEnv env) (Map.fromList funList),
            actBlockDepth = actBlockDepth env,
            actReturnType = actReturnType env
        }

checkMain :: Eval ()
checkMain = do
    env <- ask
    case Map.lookup (Ident "main") (funEnv env) of
        Just _ -> return ()
        Nothing -> throwError "\"main\" function not declared"

checkProgram :: Program -> Eval Program
checkProgram (Program topDefinitions) = do
    env <- declareBuiltIn
    env' <- local (\_ -> env) (declareFunctions topDefinitions)
    local (\_ -> env') checkMain
    optimizedTopDefs <- local (\_ -> env') (mapM checkFunction topDefinitions)
    return $ Program optimizedTopDefs
