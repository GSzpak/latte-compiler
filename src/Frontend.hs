module Frontend where


import AbsLatte
import PrintLatte
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Except
import Control.Monad.Reader
import Text.Printf
import Utils


type BlockDepth = Integer

type EnvElem = Type
type TypeEnv a = Map.Map Ident a

data VarEnvElem = VarEnvElem {
    type_ :: Type,
    blockDepth :: BlockDepth
} deriving Show

instance Typeable VarEnvElem where
    getType varEnvElem = type_ varEnvElem

data ClassEnvElem = ClassEnvElem {
    clsIdent :: Ident,
    ancestor :: Maybe ClassEnvElem,
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

type Eval a = ReaderT Env (ExceptT String IO) a

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

undeclaredClassComponentErr :: String -> Ident -> Ident -> String
undeclaredClassComponentErr s componentIdent clsIdent = 
    printf "Undeclared %s %s in class %s" s (name componentIdent) (name clsIdent)

unexpectedExprErr :: Expr -> String
unexpectedExprErr expr = printf "Unexpected expression: %s" (printTree expr)

------------------------- utils --------------------------------------------

runEval :: Env -> Eval a -> IO (Either String a)
runEval env eval = runExceptT (runReaderT eval env)

emptyEnv :: Env
emptyEnv = Env {
    varEnv = Map.empty,
    funEnv = Map.empty,
    classEnv = Map.empty,
    actBlockDepth = 0,
    actReturnType = Void
}

debug :: Show a => a -> Eval ()
debug x = liftIO $ putStrLn $ show x

incBlockDepth :: Env -> Env
incBlockDepth env = env {actBlockDepth = (actBlockDepth env) + 1}

isType :: Type -> Type -> Eval Bool
isType (Cls ident1) (Cls ident2) = do
    env <- ask
    if ident1 == ident2 then
        return True
    else do
        let Just cls = Map.lookup ident1 (classEnv env)
        case ancestor cls of
            Nothing -> return False
            Just ancestor -> isType (Cls $ clsIdent ancestor) (Cls ident2)
isType t1 t2 = return $ t1 == t2

checkTypes :: Type -> Type -> Eval Type
checkTypes actType expectedType = do
    accepted <- isType actType expectedType
    if accepted then
        return actType
    else
        throwError $ incomaptibleTypesErr expectedType actType

checkTypesEquality :: Type -> Type -> Eval Type
checkTypesEquality type1 type2 = do
    if type1 == type2 then
        return type1
    else
        throwError $ incomaptibleTypesErr type1 type2

getExistingCls :: Ident -> Eval ClassEnvElem
getExistingCls ident = do
    maybeCls <- getCls ident
    case maybeCls of
        Nothing -> throwError $ undeclaredErr "class" ident
        Just cls -> return cls

getCls :: Ident -> Eval (Maybe ClassEnvElem)
getCls ident = do
    env <- ask
    return $ Map.lookup ident (classEnv env)

getFieldType :: Ident -> Ident -> Eval Type
getFieldType clsIdent fieldIdent = do
    cls <- getExistingCls clsIdent
    case Map.lookup fieldIdent (fields cls) of
        Nothing -> throwError $ undeclaredClassComponentErr "field" fieldIdent clsIdent
        Just t -> return t

--------------------- expr types --------------------------------------------

getIdentType :: Typeable a => Ident -> (Env -> TypeEnv a) -> String -> Eval Type
getIdentType ident envSelector errMessage = do
    env <- ask
    case Map.lookup ident (envSelector env) of
        Just envElem -> return $ getType envElem
        Nothing -> throwError errMessage

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

checkEqExpr :: Expr -> Expr -> Eval Type
checkEqExpr expr1 expr2 = do
    t1 <- evalExprType expr1
    t2 <- evalExprType expr2
    checkTypesEquality t1 t2
    return Bool

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
evalExprType (EMul expr1 _ expr2) = checkTwoArgExpression expr1 expr2 [Int] Nothing
evalExprType (EAdd expr1 Plus expr2) = checkTwoArgExpression expr1 expr2 [Int, Str] Nothing
evalExprType (EAdd expr1 Minus expr2) = checkTwoArgExpression expr1 expr2 [Int] Nothing
evalExprType (ERel expr1 EQU expr2) = checkEqExpr expr1 expr2
evalExprType (ERel expr1 NE expr2) = checkEqExpr expr1 expr2
evalExprType (ERel expr1 _ expr2) = checkTwoArgExpression expr1 expr2 [Int, Bool, Str] (Just Bool)
evalExprType (EAnd expr1 expr2) = checkTwoArgExpression expr1 expr2 [Bool] Nothing
evalExprType (EOr expr1 expr2) = checkTwoArgExpression expr1 expr2 [Bool] Nothing
evalExprType (EApp ident arguments) = do
    Fun t argTypes <- getIdentType ident funEnv (undeclaredErr "function" ident)
    evalFunType ident t argTypes arguments
evalExprType (ENew ident) = do
    getExistingCls ident
    return $ Cls ident
evalExprType (ENull ident) = do
    getExistingCls ident
    return $ Cls ident
evalExprType (EMApp objIdent methodIdent arguments) = do
    identType <- evalExprType (EVar objIdent)
    case identType of
        Cls clsIdent -> (do
            cls <- getExistingCls clsIdent
            case Map.lookup methodIdent (methods cls) of
                    Nothing -> throwError $ undeclaredClassComponentErr "method" methodIdent clsIdent
                    Just (Fun t argTypes) -> evalFunType methodIdent t argTypes arguments)
        _ -> throwError $ unexpectedTypeErr identType
evalExprType (EAcc objIdent fieldIdent) = do
    objType <- evalExprType (EVar objIdent)
    case objType of
        Cls clsIdent -> getFieldType clsIdent fieldIdent
        t -> throwError $ unexpectedTypeErr t

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
        env {varEnv = Map.insert ident newVar (varEnv env)}

checkStmt :: Stmt -> Eval Env
checkStmt Empty = ask
checkStmt (BStmt block) = local incBlockDepth (checkBlock block)
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
checkStmt (Ass expr1@(EVar _) expr2) = do 
    varType <- evalExprType expr1
    valType <- evalExprType expr2
    checkTypes valType varType
    ask
checkStmt (Ass expr1@(EAcc _ _) expr2) = do 
    varType <- evalExprType expr1
    valType <- evalExprType expr2
    checkTypes valType varType
    ask
checkStmt (Ass expr _) = throwError $ unexpectedExprErr expr
checkStmt (Incr expr) = do
    exprType <- evalExprType expr
    checkTypes exprType Int
    ask
checkStmt (Decr expr) = checkStmt (Incr expr)
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

----------------- Constants folding ---------------------------------------

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
foldConstants (Ass expr1 expr2) = do 
    folded <- foldConstExpr expr2
    return $ Ass expr1 folded
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

---------------------------- Statements optimizations -------------------------

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

-------------------- Declarations -------------------------------------------

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
        local (\env -> env {funEnv = Map.union (funEnv env) (Map.fromList builtIn)}) ask

checkForDuplicates :: [Ident] -> String -> Eval ()
checkForDuplicates [] _ = return ()
checkForDuplicates (ident:idents) typeName =
    if ident `elem` idents then
        throwError $ duplicatedErr typeName ident
    else
        checkForDuplicates idents typeName

declareCls :: Ident -> Maybe ClassEnvElem -> [Field] -> [FnDef] -> Eval Env
declareCls ident ancestor fieldList methodList = do
    (ancestorFields, ancestorMethods) <- case ancestor of
        Nothing -> return (Map.empty, Map.empty)
        Just cls -> return (fields cls, methods cls)
    let fieldIdents = map getIdent fieldList
    let methodIdents = map (\(FnDef _ ident _ _) -> ident) methodList
    checkForDuplicates fieldIdents "field"
    checkForDuplicates methodIdents "method"
    let actFields = Map.fromList (map (\(Field t ident) -> (ident, t)) fieldList)
    let actMethods = Map.fromList (map (\fun@(FnDef _ ident _ _) -> (ident, getType fun)) methodList)
    let clsFields = Map.union actFields ancestorFields
    let clsMethods = Map.union actMethods ancestorMethods
    local (addCls clsFields clsMethods) ask
    where
        addCls :: TypeEnv EnvElem -> TypeEnv EnvElem -> Env -> Env
        addCls clsFields clsMethods env =
            let
                newCls = ClassEnvElem {
                    clsIdent = ident,
                    ancestor = ancestor,
                    fields = clsFields,
                    methods = clsMethods
                }
            in env {classEnv = Map.insert ident newCls (classEnv env)}

declareClass :: Ident -> Maybe ClassEnvElem -> [Field] -> [FnDef] -> Eval Env
declareClass ident ancestor fields methods = 
    (declareCls ident ancestor fields methods)
    `catchError` 
    (\message -> throwError $ printf "%s in class: %s" message (name ident))

declareTopDefs :: [TopDef] -> Eval Env
declareTopDefs [] = ask
declareTopDefs ((FnTopDef fun@(FnDef t ident _ _)):defs) = do
    checkIfFunDuplicated ident
    local (\env -> env {funEnv = Map.insert ident (getType fun) (funEnv env)}) (declareTopDefs defs)
declareTopDefs ((ClsDef ident fields methods):defs) = do
    checkIfClsDuplicated ident
    env' <- declareClass ident Nothing fields methods
    local (\_ -> env') (declareTopDefs defs)
declareTopDefs ((ClsExtDef ident ancestorId fields methods):defs) = do
    checkIfClsDuplicated ident
    ancestor <- getExistingCls ancestorId
    env' <- declareClass ident (Just ancestor) fields methods
    local (\_ -> env') (declareTopDefs defs)

--------------------------- Checking correctness ---------------------------

-- Checked after deleting unreachable code
hasReturn :: Stmt -> Bool
hasReturn (Ret _) = True
hasReturn VRet = True
hasReturn (CondElse _ stmt1 stmt2) = (hasReturn stmt1) && (hasReturn stmt2)
hasReturn (BStmt (Block stmts)) = any hasReturn stmts
hasReturn _ = False

checkIfTypeDeclared ::  Type -> Eval ()
checkIfTypeDeclared (Cls ident) = do
    _ <- getExistingCls ident
    return ()
checkIfTypeDeclared _ = return ()

checkIfFunDuplicated :: Ident -> Eval ()
checkIfFunDuplicated ident = do
    env <- ask
    case Map.lookup ident (funEnv env) of
        Just _ -> throwError $ duplicatedErr "function" ident
        Nothing -> return ()

checkIfClsDuplicated :: Ident -> Eval ()
checkIfClsDuplicated ident = do
    cls <- getCls ident
    case cls of
        Just _ -> throwError $ duplicatedErr "class" ident
        Nothing -> return ()

checkFun :: FnDef -> Eval FnDef
checkFun (FnDef t ident args block)  = do
    env <- ask
    let argTypes = map getType args
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
        prepareBlockCheck env = env {
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

checkCls :: Ident -> [Field] -> [FnDef] -> Eval [FnDef]
checkCls clsIdent fields methods = do
    env <- addFieldsToEnv fields
    optimizedMethods <- local (\_ -> incBlockDepth env) (mapM checkFunction (map addSelfArg methods))
    return optimizedMethods
    where
        addSelfArg :: FnDef -> FnDef
        addSelfArg (FnDef t ident args block) = FnDef t ident ((selfArg clsIdent):args) block
        addFieldsToEnv :: [Field] -> Eval Env
        addFieldsToEnv [] = ask
        addFieldsToEnv ((Field t ident):fields) =
            local (declareVar ident t) (addFieldsToEnv fields)

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
