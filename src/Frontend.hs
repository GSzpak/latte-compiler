module Frontend where


import AbsLatte
import PrintLatte
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Error
import Control.Monad.Reader
import Text.Printf


type BlockDepth = Integer

type EnvElem = Type
type TypeEnv a = Map.Map Ident a

data VarEnvElem = VarEnvElem {
    type_ :: Type,
    blockDepth :: BlockDepth
} deriving Show

data ClassEnvElem = ClassEnvElem {
    ancestors :: Set.Set Ident,
    fields :: TypeEnv EnvElem,
    methods :: TypeEnv EnvElem
} deriving Show

type VarEnv = TypeEnv VarEnvElem
type FunEnv = TypeEnv EnvElem
type ClassEnv = TypeEnv ClassEnvElem

data Env = Env {
    varEnv :: VarEnv,
    funEnv :: FunEnv,
    classEnv :: ClassEnv,
    actBlockDepth :: BlockDepth,
    actReturnType :: Type
} deriving Show

type Eval a = ReaderT Env (ErrorT String IO) a


class Typeable a where
    getType :: a -> Type

instance Typeable Type where
    getType t = t

instance Typeable VarEnvElem where
    getType varEnvElem = type_ varEnvElem

instance Typeable Arg where
    getType (Arg type_ _) = type_

instance Typeable FnDef where
    getType (FnDef t ident args _) = Fun t (map getType args)


runEval :: Env -> Eval a -> IO (Either String a)
runEval env eval = runErrorT (runReaderT eval env)

emptyEnv :: Env
emptyEnv = Env {
    varEnv = Map.empty,
    funEnv = Map.empty,
    classEnv = Map.empty,
    actBlockDepth = 0,
    actReturnType = Void
}

name :: Ident -> String
name (Ident s) = s

isType :: Type -> Type -> Eval Bool
isType (Cls ident1) (Cls ident2) = do
    env <- ask
    let Just cls = Map.lookup ident1 (classEnv env)
    return $ Set.member ident2 (ancestors cls)
isType t1 t2 = return $ t1 == t2

---------------- errors ---------------------------------------------

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

undeclaredErr :: String -> Ident -> String
undeclaredErr s ident = printf "Undeclared %s: %s" s (name ident)

duplicatedErr :: String -> Ident -> String
duplicatedErr s ident = printf "Duplicated %s declaration: %s" s (name ident)

returnInVoidErr :: Expr -> String
returnInVoidErr expr = printf "Returning expression: %s in a void function" (printTree expr)

intRetTypeErr :: Type -> String
intRetTypeErr t = printf "Invalid return type of \"main\" function: expected %s, got %s" (printTree Int) (printTree t)

undeclaredMethodErr :: Ident -> Ident -> String
undeclaredMethodErr clsIdent methodIdent = 
    printf "Undeclared method %s in class %s" (name clsIdent) (name methodIdent)

--------------------- expr types --------------------------------------------

getIdentType :: Typeable a => Ident -> (Env -> TypeEnv a) -> String -> Eval Type
getIdentType ident envSelector errMessage = do
    env <- ask
    case Map.lookup ident (envSelector env) of
        Just envElem -> return $ getType envElem
        Nothing -> throwError errMessage

checkTypes :: Type -> Type -> Eval Type
checkTypes actType expectedType = do
    typesEqual <- isType actType expectedType
    if typesEqual then
        return actType
    else
        throwError $ incomaptibleTypesErr expectedType actType

checkExprType :: Expr -> Type -> Eval Type
checkExprType expr expectedType = do
    exprType <- evalExprType expr
    checkTypes exprType expectedType

checkTwoArgExpression :: Expr -> Expr -> [Type] -> Maybe Type -> Eval Type
checkTwoArgExpression expr1 expr2 expectedTypes retType = do
    t1 <- evalExprType expr1
    t2 <- evalExprType expr2
    checkTypes t1 t2
    -- t1 == t2
    let resultType = t1
    if resultType `elem` expectedTypes then
        return $ case retType of
            Nothing -> resultType
            Just t -> t
    else
        throwError $ unexpectedTypeErr resultType

evalFunType :: Ident -> Type -> [Type] -> [Expr] -> Eval Type
evalFunType ident retType argTypes arguments = do
    actTypes <- mapM evalExprType arguments
    if length actTypes /= length argTypes then
        throwError $ printf "Wrong number of arguments in function call: %s" (name ident)
    else do
        let funTypes = zip actTypes argTypes
        typesEqual <- mapM (uncurry isType) funTypes
        if all (== True) typesEqual then
            return retType
        else
            throwError $ incompatibleParametersErr ident argTypes actTypes 

evalExprType :: Expr -> Eval Type
evalExprType (EVar ident) = getIdentType ident varEnv (undeclaredErr "variable" ident)
evalExprType (ELitInt _) = return Int
evalExprType ELitTrue = return Bool
evalExprType ELitFalse = return Bool
evalExprType (EString _) = return Str
evalExprType (Neg expr) = checkExprType expr Int
evalExprType (Not expr) = checkExprType expr Bool
evalExprType e@(EMul expr1 _ expr2) = checkTwoArgExpression expr1 expr2 [Int] Nothing
evalExprType e@(EAdd expr1 Plus expr2) = checkTwoArgExpression expr1 expr2 [Int, Str] Nothing
evalExprType e@(EAdd expr1 Minus expr2) = checkTwoArgExpression expr1 expr2 [Int] Nothing
evalExprType e@(ERel expr1 _ expr2) = checkTwoArgExpression expr1 expr2 [Int, Bool, Str] (Just Bool)
evalExprType e@(EAnd expr1 expr2) = checkTwoArgExpression expr1 expr2 [Bool] Nothing
evalExprType e@(EOr expr1 expr2) = checkTwoArgExpression expr1 expr2 [Bool] Nothing
evalExprType e@(EApp ident arguments) = do
    Fun t argTypes <- getIdentType ident funEnv (undeclaredErr "function" ident)
    evalFunType ident t argTypes arguments
evalExprType (ENew ident) = do
    checkIfClsDeclared ident
    return $ Cls ident
evalExprType (ENull ident) = do
    checkIfClsDeclared ident
    return $ Cls ident
evalExprType (EMApp clsIdent methodIdent arguments) = do
    maybeCls <- getCls clsIdent
    case maybeCls of
        Nothing -> throwError $ undeclaredErr "class" clsIdent
        Just cls -> case Map.lookup methodIdent (methods cls) of
            Nothing -> throwError $ undeclaredMethodErr clsIdent methodIdent
            Just (Fun t argTypes) -> evalFunType methodIdent t argTypes arguments

checkIfVarDeclared :: Ident -> Eval ()
checkIfVarDeclared ident = do
    env <- ask
    case Map.lookup ident (varEnv env) of
        Just (VarEnvElem t depth) ->
            (if (actBlockDepth env) <= depth then
                throwError $ duplicatedErr "variable" ident
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
            classEnv = classEnv env,
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
            classEnv = classEnv env,
            actBlockDepth = (actBlockDepth env) + 1,
            actReturnType = actReturnType env
        }
checkStmt (Decl t items) = do 
    checkIfTypeDeclared t
    if t == Void then
        throwError $ printf "Invalid variable type: %s" (printTree Void)
    else
        checkDeclaration items
    where
        checkDeclaration :: [Item] -> Eval Env
        checkDeclaration [] = ask
        checkDeclaration ((NoInit ident):items) = do
            checkIfVarDeclared ident
            local (declareVar ident t) (checkDeclaration items)
        checkDeclaration ((Init ident expr):items) = do
            checkIfVarDeclared ident
            checkExprType expr t
            local (declareVar ident t) (checkDeclaration items)
checkStmt s@(Ass ident expr) = do 
    identType <- getIdentType ident varEnv (undeclaredErr "variable" ident)
    checkExprType expr identType
    ask
checkStmt s@(Incr ident) = do
    identType <- getIdentType ident varEnv (undeclaredErr "variable" ident)
    checkTypes identType Int
    ask
checkStmt s@(Decr ident) = do
    identType <- getIdentType ident varEnv (undeclaredErr "variable" ident)
    checkTypes identType Int
    ask
checkStmt (Ret expr) = do
    env <- ask
    if actReturnType env == Void then
        throwError $ returnInVoidErr expr
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
checkStmt (SExp expr) = do
    evalExprType expr
    ask

checkStatement :: Stmt -> Eval Env
checkStatement stmt =
    (checkStmt stmt)
    `catchError` 
    (\message -> throwError $ printf "%s\nin statement: %s" message (printTree stmt))

checkStatements :: [Stmt] -> Eval Env
checkStatements [] = ask
checkStatements (stmt:statements) = do
    env <- checkStatement stmt
    local (\_ -> env) (checkStatements statements)

checkBlock :: Block -> Eval Env
checkBlock (Block statements) = checkStatements statements

-- Expressions folding is run after type checking

isConstant :: Expr -> Bool
isConstant (ELitInt _) = True
isConstant ELitTrue = True
isConstant ELitFalse = True
isConstant (EString s) = True
isConstant _ = False

exprFromBool :: Bool -> Expr
exprFromBool True = ELitTrue
exprFromBool False = ELitFalse

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

foldRelExpr :: Expr -> Expr -> (Expr -> Expr -> Expr) -> (Expr -> Expr -> Bool) -> Eval Expr
foldRelExpr expr1 expr2 constructor relOper = do
    folded1 <- foldConstExpr expr1
    folded2 <- foldConstExpr expr2
    if ((isConstant folded1) && (isConstant folded2)) then
        return $ exprFromBool $ relOper folded1 folded2
    else
        return $ constructor folded1 folded2

foldConstExpr :: Expr -> Eval Expr
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
foldConstExpr (EAnd e1 e2) = do
    folded1 <- foldConstExpr e1
    folded2 <- foldConstExpr e2
    case (folded1, folded2) of
        (_, ELitFalse) -> return ELitFalse
        (ELitFalse, _) -> return ELitFalse
        (_, ELitTrue) -> return folded1
        (ELitTrue, _) -> return folded2
        (_, _) -> return $ EAnd folded1 folded2
foldConstExpr (EOr e1 e2) = do
    folded1 <- foldConstExpr e1
    folded2 <- foldConstExpr e2
    case (folded1, folded2) of
        (_, ELitTrue) -> return ELitTrue
        (ELitTrue, _) -> return ELitTrue
        (_, ELitFalse) -> return folded1
        (ELitFalse, _) -> return folded2
        (_, _) -> return $ EOr folded1 folded2
foldConstExpr (ERel e1 LTH e2) = foldRelExpr e1 e2 ((flip ERel) LTH) (<)
foldConstExpr (ERel e1 LE e2) = foldRelExpr e1 e2 ((flip ERel) LE) (<=)
foldConstExpr (ERel e1 GTH e2) = foldRelExpr e1 e2 ((flip ERel) GTH) (>)
foldConstExpr (ERel e1 GE e2) = foldRelExpr e1 e2 ((flip ERel) GE) (>=)
foldConstExpr (ERel e1 EQU e2) = foldRelExpr e1 e2 ((flip ERel) EQU) (==)
foldConstExpr (ERel e1 NE e2) = foldRelExpr e1 e2 ((flip ERel) NE) (/=)
foldConstExpr (EApp ident exprs) = do
    foldedArgs <- mapM foldConstExpr exprs
    return $ EApp ident foldedArgs
foldConstExpr (EMApp clsIdent methodIdent exprs) = do
    foldedArgs <- mapM foldConstExpr exprs
    return $ EMApp clsIdent methodIdent foldedArgs
foldConstExpr expr = return expr

-- TODO: remove
debug :: Show a => a -> Eval ()
debug x = liftIO $ putStrLn $ show x

foldConstants :: Stmt -> Eval Stmt
foldConstants (BStmt block) = do
    foldedBlock <- foldConstantsInBlock block
    return $ BStmt foldedBlock
foldConstants (Decl t items) = do
    foldedItems <- mapM foldItem items
    return (Decl t foldedItems)
    where
        foldItem :: Item -> Eval Item
        foldItem (NoInit ident) = return $ NoInit ident
        foldItem (Init ident expr) = do
            foldedExpr <- foldConstExpr expr
            return $ Init ident foldedExpr
foldConstants (Ass ident expr) = do 
    folded <- foldConstExpr expr
    return $ Ass ident folded
foldConstants (Ret expr) = do
    folded <- foldConstExpr expr
    return $ Ret folded
foldConstants (Cond expr stmt) = do
    foldedStmt <- foldConstants stmt
    foldedExpr <- foldConstExpr expr
    return $ Cond foldedExpr foldedStmt
foldConstants (CondElse expr stmt1 stmt2) = do
    foldedStmt1 <- foldConstants stmt1
    foldedStmt2 <- foldConstants stmt2
    foldedExpr <- foldConstExpr expr
    return $ CondElse foldedExpr foldedStmt1 foldedStmt2
foldConstants (While expr stmt) = do
    foldedStmt <- foldConstants stmt
    foldedExpr <- foldConstExpr expr
    return $ While foldedExpr foldedStmt
foldConstants (SExp expr) = do
    folded <- foldConstExpr expr
    return $ SExp folded
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
hasReturn (BStmt (Block stmts)) = any hasReturn stmts
hasReturn _ = False

checkIfClsDeclared :: Ident -> Eval ()
checkIfClsDeclared ident = do
    maybeCls <- getCls ident
    case maybeCls of
        Just _ -> return ()
        Nothing -> throwError $ undeclaredErr "class" ident

checkIfTypeDeclared ::  Type -> Eval ()
checkIfTypeDeclared (Cls ident) = checkIfClsDeclared ident
checkIfTypeDeclared _ = return ()

checkFun :: FnDef -> Eval FnDef
checkFun (FnDef t ident args block) = do
    env <- ask
    let Just (Fun t argTypes) = Map.lookup ident (funEnv env)
    sequence_  $ map checkIfTypeDeclared (t:argTypes)
    checkIfAnyVoid argTypes
    env <- declareArgs args
    let env' = prepareBlockCheck env
    local (\_ -> env') (checkBlock block)
    foldedConstantsBlock <- foldConstantsInBlock block
    let (Block optimized) = optimizeBlock foldedConstantsBlock
    if (t /= Void) && (not (any hasReturn optimized)) then
        throwError $ "No \"return\" instruction"
    else
        return $ FnDef t ident args (Block optimized)
    where
        prepareBlockCheck :: Env -> Env
        prepareBlockCheck env = Env {
            varEnv = varEnv env,
            funEnv = funEnv env,
            classEnv = classEnv env,
            actBlockDepth = (actBlockDepth env) + 1,
            actReturnType = t
        }
        declareArgs :: [Arg] -> Eval Env
        declareArgs [] = ask
        declareArgs ((Arg type_ ident):args) = do
            checkIfVarDeclared ident
            local (declareVar ident type_) (declareArgs args)
        checkIfAnyVoid :: [Type] -> Eval ()
        checkIfAnyVoid types = do
            if any (== Void) types then
                throwError $ "Argument of type void"
            else
                return ()

checkFunction :: FnDef -> Eval FnDef
checkFunction fun@(FnDef _ ident _ _) =
    (checkFun fun) 
    `catchError` 
    (\message -> throwError $ printf "%s in function: %s" message (name ident))

addFieldsToEnv :: [Field] -> Eval Env
addFieldsToEnv [] = ask
addFeildsToEnv ((Field t ident):fields) =
    local (declareVar ident t) (addFieldsToEnv fields)

checkCls :: Ident -> [Field] -> [FnDef] -> Eval [FnDef]
checkCls ident fields methods = do
    env <- addFieldsToEnv fields
    optimizedMethods <- local (\_ -> env) (mapM checkFunction methods)
    return optimizedMethods

checkClass :: Ident -> [Field] -> [FnDef] -> Eval [FnDef]
checkClass ident fields methods =
    (checkCls ident fields methods)
    `catchError` 
    (\message -> throwError $ printf "%s in class: %s" message (name ident))

checkTopDef :: TopDef -> Eval TopDef
checkTopDef (FnTopDef fnDef) = do
    checkedFun <- checkFunction fnDef
    return $ FnTopDef checkedFun
checkTopDef (ClsDef ident fields methods) = do
    checkedMethods <- checkClass ident fields methods
    return $ ClsDef ident fields checkedMethods
checkTopDef (ClsExtDef ident ancestor fields methods) = do
    checkedMethods <- checkClass ident fields methods
    return $ ClsExtDef ident ancestor fields checkedMethods

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
            classEnv = classEnv env,
            actBlockDepth = actBlockDepth env,
            actReturnType = actReturnType env
        }


declareFields :: [Field] -> TypeEnv EnvElem -> Eval (TypeEnv EnvElem)
declareFields [] accu = return accu
declareFields ((Field t ident):fields) accu =
    case Map.lookup ident accu of
        Nothing -> declareFields fields (Map.insert ident t accu)
        Just _ -> throwError $ duplicatedErr "field" ident

declareMethods :: [FnDef] -> TypeEnv EnvElem -> Eval (TypeEnv EnvElem)
declareMethods [] accu = return accu
declareMethods ((FnDef t ident _ _):methods) accu =
    case Map.lookup ident accu of
        Nothing -> declareMethods methods (Map.insert ident t accu)
        Just _ -> throwError $ duplicatedErr "method" ident

declareCls :: Ident -> Set.Set Ident -> [Field] -> [FnDef] -> Eval Env
declareCls ident ancestors fields methods = do
    clsFields <- declareFields fields Map.empty
    clsMethods <- declareMethods methods Map.empty
    local (addCls clsFields clsMethods) ask
    where
        addCls :: TypeEnv EnvElem -> TypeEnv EnvElem -> Env -> Env
        addCls clsFields clsMethods env =
            let
                newCls = ClassEnvElem {
                    ancestors = ancestors,
                    fields = clsFields,
                    methods = clsMethods
                }
            in Env {
                varEnv = varEnv env,
                funEnv = funEnv env,
                classEnv = Map.insert ident newCls (classEnv env),
                actBlockDepth = actBlockDepth env,
                actReturnType = actReturnType env
            }

declareClass :: Ident -> Set.Set Ident -> [Field] -> [FnDef] -> Eval Env
declareClass ident ancestors fields methods = 
    (declareCls ident ancestors fields methods)
    `catchError` 
    (\message -> throwError $ printf "%s in class: %s" message (name ident))

checkIfFunDuplicated :: Ident -> Eval ()
checkIfFunDuplicated ident = do
    env <- ask
    case Map.lookup ident (funEnv env) of
        Just _ -> throwError $ duplicatedErr "function" ident
        Nothing -> return ()

getCls :: Ident -> Eval (Maybe ClassEnvElem)
getCls ident = do
    env <- ask
    return $ Map.lookup ident (classEnv env)

checkIfClsDuplicated :: Ident -> Eval ()
checkIfClsDuplicated ident = do
    cls <- getCls ident
    case cls of
        Just _ -> throwError $ duplicatedErr "class" ident
        Nothing -> return ()

getAncestors :: Ident -> Eval (Set.Set Ident)
getAncestors ancestor = do
    maybeAncestorCls <- getCls ancestor
    case maybeAncestorCls of
        Just ancestorCls -> return $ Set.insert ancestor (ancestors ancestorCls)
        Nothing -> throwError $ undeclaredErr "class" ancestor

declareTopDefs :: [TopDef] -> Eval Env
declareTopDefs [] = ask
declareTopDefs ((FnTopDef fun@(FnDef t ident _ _)):defs) = do
    checkIfFunDuplicated ident
    local declareFun (declareTopDefs defs)
    where
        declareFun :: Env -> Env
        declareFun env = Env {
            varEnv = varEnv env,
            funEnv = Map.insert ident (getType fun) (funEnv env),
            classEnv = classEnv env,
            actBlockDepth = actBlockDepth env,
            actReturnType = actReturnType env
        }
declareTopDefs ((ClsDef ident fields methods):defs) = do
    checkIfClsDuplicated ident
    env' <- declareClass ident Set.empty fields methods
    local (\_ -> env') (declareTopDefs defs)
declareTopDefs ((ClsExtDef ident ancestor fields methods):defs) = do
    checkIfClsDuplicated ident
    ancestors <- getAncestors ancestor
    env' <- declareClass ident ancestors fields methods
    local (\_ -> env') (declareTopDefs defs)

checkMain :: Eval ()
checkMain = do
    env <- ask
    case Map.lookup (Ident "main") (funEnv env) of
        Just (Fun Int []) -> return ()
        Just (Fun Int _) ->  throwError "\"main\" function should not take any arguments"
        Just (Fun t _) ->  throwError $ intRetTypeErr t
        Nothing -> throwError "\"main\" function not declared"

checkProgram :: Program -> Eval Program
checkProgram (Program topDefinitions) = do
    env <- declareBuiltIn
    env' <- local (\_ -> env) (declareTopDefs topDefinitions)
    local (\_ -> env') checkMain
    optimizedTopDefs <- local (\_ -> env') (mapM checkTopDef topDefinitions)
    return $ Program optimizedTopDefs
