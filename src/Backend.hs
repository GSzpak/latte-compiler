module Backend where


import AbsLatte
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Control.Monad.Reader
import Control.Monad.State
import Text.Printf
import Utils


type Name = String
type Registry = Name
type Counter = Integer
type Label = String
type BlockNum = Integer

data ExpValRepr =
    RegVal Name |
    NumVal Integer |
    BoolVal Bool |
    NullVal

data ExpVal = ExpVal {
    repr :: ExpValRepr,
    type_ :: Type
}

data LLVMBlock = LLVMBlock {
    labelNum :: BlockNum,
    instructions :: [Instruction],
    lastInstr :: Maybe Instruction
} deriving Show

data VtableElem  = VtableElem {
    methodIdent :: Ident,
    pointerName :: Name,
    pointerType :: Type
}

type Vtable = Vector.Vector VtableElem

data LatteClass = LatteClass {
    clsIdent :: Ident,
    fields :: Vector.Vector Field,
    ancestor :: Maybe LatteClass,
    vtable :: Vtable
}

data EnvElem = EnvElem Name Type deriving Show

type Env = Map.Map Ident EnvElem

data Environment = Environment {
    varEnv :: Env,
    funEnv :: Env,
    actClass :: Maybe LatteClass
} deriving Show

data Store = Store {
    blocks :: Map.Map BlockNum LLVMBlock,
    actBlockNum :: BlockNum,
    regCounter :: Counter,
    constCounter :: Counter,
    labelCounter :: Counter,
    strConstants :: Map.Map String Name,
    classes :: Map.Map Ident LatteClass,
    compiled :: [String]
} deriving Show

type Eval a = ReaderT Environment (StateT Store IO) a

data BinOp = Add | Sub | Mul | DivOp | ModOp

data Instruction =
    BinOpExpr Registry BinOp ExpVal ExpVal |
    RelOpExpr Registry RelOp ExpVal ExpVal |
    NotExpr Registry ExpVal |
    Load Registry Type Registry |
    Call Registry Type Name [ExpVal] |
    StoreInstr ExpVal Registry |
    Alloca Registry Type |
    VoidRet |
    ExpRet ExpVal |
    CondJump ExpVal BlockNum BlockNum |
    Jump BlockNum |
    Phi Registry Type [(ExpVal, BlockNum)] |
    GetElementPtr Registry Type Name Integer |
    FunDecl Name Type [Type] |
    Bitcast Registry ExpVal Type

--- printing utils -------------------------------------

identReg :: Ident -> Registry
identReg (Ident name) = '%':name

regName :: Counter -> Registry
regName regNum = "%r" ++ (show regNum)

globalName :: Ident -> Name
globalName (Ident name) = '@':name

constName :: Counter -> Name
constName strNum = printf "@.str%s" (show strNum)

classRepr :: Ident -> Registry
classRepr (Ident clsName) = "%class." ++ clsName

methodName :: Ident -> Ident -> Name
methodName (Ident methodId) (Ident clsId) = printf "%s.class.%s" clsId methodId

vtableName :: Ident -> Name
vtableName (Ident clsId) = printf "@%s.class.vtable" clsId

label :: BlockNum -> Label
label num = printf "label%s" (show num)

llvmStrLen :: String -> Integer
llvmStrLen s = (toInteger $ length s) + 1

showSigned :: RelOp -> String
showSigned LTH = "slt"
showSigned LE = "sle"
showSigned GTH = "sgt"
showSigned GE = "sge"
showSigned EQU = "eq"
showSigned NE = "ne"

showUnsigned :: RelOp -> String
showUnsigned LTH = "ult"
showUnsigned LE = "ule"
showUnsigned GTH = "ugt"
showUnsigned GE = "uge"
showUnsigned EQU = "eq"
showUnsigned NE = "ne"

showLLVMType :: Type -> String
showLLVMType Int = "i32"
showLLVMType Str = "i8*"
showLLVMType Bool = "i1"
showLLVMType Void = "void"
showLLVMType Char = "i8"
showLLVMType (Cls ident) = printf "%s*" (classRepr ident)
showLLVMType (Ptr t) = printf "%s*" (showLLVMType t)
showLLVMType (Arr len t) = printf "[%s x %s]" (show len) (showLLVMType t)
showLLVMType (Fun retType argTypes) =
    printf "%s (%s)" (showLLVMType retType) (printWithSeparator (map showLLVMType argTypes) ",")
showLLVMType (VtableType (Ident id)) = "%class." ++ (printf "%s.vtableType" id)

showClsRepr :: LatteClass -> String
showClsRepr cls = classRepr (clsIdent cls)

printWithSeparator :: [String] -> String -> String
printWithSeparator strings sep = unwords $ intersperse sep strings

showFunArgs :: [ExpVal] -> String
showFunArgs args = printWithSeparator (map show args) ","

showPhiExprs :: [(ExpVal, BlockNum)] -> String
showPhiExprs exprsWithLabels = printWithSeparator (map show' exprsWithLabels) ","
    where
        show' :: (ExpVal, BlockNum) -> String
        show' (val, num) = printf "[ %s, %s ]" (show $ repr val) ('%':(label num))

instance Show ExpValRepr where
    show (RegVal name) = name
    show (NumVal n) = show n
    show (BoolVal True) = "true"
    show (BoolVal False) = "false"
    show NullVal = "null"

instance Show ExpVal where
    show val = printf "%s %s" (showLLVMType $ type_ val) (show $ repr val)

instance Show VtableElem where
    show vtableElem = printf "%s %s" (showLLVMType $ pointerType vtableElem) (pointerName vtableElem)

instance Show LatteClass where
    show cls =
        let
            fieldsReprs = printWithSeparator (map (showLLVMType . getType) (Vector.toList $ fields cls)) ","
            vtableType = showLLVMType $ Ptr $ VtableType (clsIdent cls)
        in
            case fieldsReprs of
                "" -> printf "%s = type { %s }" (showClsRepr cls) vtableType
                _ -> printf "%s = type { %s, %s }" (showClsRepr cls) vtableType fieldsReprs

instance Show BinOp where
    show Add = "add"
    show Sub = "sub"
    show Mul = "mul"
    show DivOp = "sdiv"
    show ModOp = "srem"

instance Show Instruction where
    show (BinOpExpr result binop val1 val2) =
        let
            typeRepr = showLLVMType $ type_ val1
            val1Repr = show $ repr val1
            val2Repr = show $ repr val2
        in
            printf "%s = %s %s %s, %s" result (show binop) typeRepr val1Repr val2Repr
    show (RelOpExpr result relOp val1 val2) =
        let
            t = type_ val1
            typeRepr = showLLVMType $ t
            val1Repr = show $ repr val1
            val2Repr = show $ repr val2
            relOpRepr = if t == Int then showSigned relOp else showUnsigned relOp
        in
            printf "%s = icmp %s %s %s, %s" result relOpRepr typeRepr val1Repr val2Repr
    show (NotExpr result val) =
        printf "%s = xor i1 %s, true" result (show $ repr val)
    show (Load result t reg) = 
        printf "%s = load %s* %s" result (showLLVMType t) reg
    show (Call _ Void fun args) = 
       printf "call %s %s(%s)" (showLLVMType Void) fun (showFunArgs args)
    show (Call result t fun args) = 
       printf "%s = call %s %s(%s)" result (showLLVMType t) fun (showFunArgs args)
    show (StoreInstr val reg) =
        printf "store %s, %s* %s" (show val) (showLLVMType $ type_ val) reg
    show (Alloca reg t) =
        printf "%s = alloca %s" reg (showLLVMType t)
    show VoidRet = "ret void"
    show (ExpRet val) = printf "ret %s" (show val)
    show (CondJump val labelNum1 labelNum2) =
        printf "br %s, label %s, label %s" (show val) ('%':(label labelNum1)) ('%':(label labelNum2))
    show (Jump labelNum) = printf "br label %s" ('%':(label labelNum))
    show (Phi result t exprsFromLabels) = 
        printf "%s = phi %s %s" result (showLLVMType t) (showPhiExprs exprsFromLabels)
    show (GetElementPtr resultReg t name index) = 
        printf "%s = getelementptr inbounds %s %s, i32 0, i32 %s" resultReg (showLLVMType t) name (show index)
    show (FunDecl name retType argTypes) =
        let
            showArgTypes = printWithSeparator (map showLLVMType argTypes) ","
        in 
            printf "declare %s @%s(%s)" (showLLVMType retType) name showArgTypes
    show (Bitcast resultReg val t) =
        printf "%s = bitcast %s to %s" resultReg (show val) (showLLVMType t)

showVtableType :: Vtable -> Ident -> String
showVtableType vtable ident =
    let
        funTypes = printWithSeparator (map (showLLVMType . pointerType) (Vector.toList vtable)) ","
    in 
        printf "%s = type { %s }" (showLLVMType $ VtableType ident) funTypes

showVtable :: Vtable -> Ident -> String
showVtable vtable ident = 
    let
        vtableElems = printWithSeparator (map show (Vector.toList vtable)) ","
    in
        printf "%s = internal constant %s { %s }" (vtableName ident) (showLLVMType $ VtableType ident) vtableElems

showLLVMArgs :: [Arg] -> String
showLLVMArgs args = printWithSeparator (map showLLVMArg args) ","
    where
        showLLVMArg :: Arg -> String
        showLLVMArg (Arg type_ ident) = printf "%s %s" (showLLVMType type_) (identReg ident)

------------------------ utils ----------------------------------------------

instance Typeable LatteClass where
    getType cls = Cls (clsIdent cls)

emptyEnv :: Environment
emptyEnv = Environment {
    varEnv = Map.empty,
    funEnv = Map.empty,
    actClass = Nothing
}

emptyStore :: Store
emptyStore = Store {
    blocks = Map.empty,
    actBlockNum = 0,
    regCounter = 0,
    constCounter = 0,
    labelCounter = 0,
    strConstants = Map.empty,
    classes = Map.empty,
    compiled = []
}

runEval :: Environment -> Store -> Eval a -> IO (a, Store)
runEval env store eval = runStateT (runReaderT eval env) store

numExpVal :: Integer -> ExpVal
numExpVal n = ExpVal {
    repr = NumVal n,
    type_ = Int
}

boolExpVal :: Bool -> ExpVal
boolExpVal b = ExpVal {
    repr = BoolVal b,
    type_ = Bool
}

trueExpVal :: ExpVal
trueExpVal = boolExpVal True

falseExpVal :: ExpVal
falseExpVal = boolExpVal False

defaultVal :: Type -> Eval ExpVal
defaultVal t = case t of
    Int -> return $ numExpVal 0
    Bool -> return falseExpVal
    Str -> emitExpr $ EString ""
    _ -> return ExpVal {repr = NullVal, type_ = t}

getNextRegistry :: Eval Registry
getNextRegistry = do
    store <- get
    let actRegNum = regCounter store
    put $ store {regCounter = actRegNum + 1}
    return $ regName actRegNum

addToBlock :: [Instruction] -> LLVMBlock -> LLVMBlock
addToBlock instrs block = LLVMBlock {
    labelNum = labelNum block,
    instructions = instrs ++ (instructions block),
    lastInstr = lastInstr block
}

setLastInBlock :: Instruction -> LLVMBlock -> LLVMBlock
setLastInBlock lastInstr block = block {lastInstr = Just lastInstr}

getBlock :: BlockNum -> Eval LLVMBlock
getBlock labelNum = do
    store <- get
    let Just block = Map.lookup labelNum (blocks store)
    return block
    
getActBlock :: Eval LLVMBlock
getActBlock = do
    store <- get
    let Just actBlock = Map.lookup (actBlockNum store) (blocks store)
    return actBlock

getActBlockNum :: Eval BlockNum
getActBlockNum = do
    store <- get
    return $ actBlockNum store

setActBlockNum :: BlockNum -> Eval ()
setActBlockNum blockNum = do
    store <- get
    put $ store {actBlockNum = blockNum}

changeBlock :: BlockNum -> (LLVMBlock -> LLVMBlock) -> Eval ()
changeBlock blockNum updateFun = do
    store <- get
    let Just actBlock = Map.lookup blockNum (blocks store)
    put $ store {blocks = Map.insert blockNum (updateFun actBlock) (blocks store)}

addInstructions :: [Instruction] -> Eval ()
addInstructions instrs = do
    store <- get
    changeBlock (actBlockNum store) (addToBlock instrs)

addInstruction :: Instruction -> Eval ()
addInstruction instr = addInstructions [instr]

setLastInstructionInBlock :: Instruction -> BlockNum -> Eval ()
setLastInstructionInBlock lastInstr labelNum = 
    changeBlock labelNum (setLastInBlock lastInstr)

setLastInstruction :: Instruction -> Eval ()
setLastInstruction lastInstr = do
    store <- get
    setLastInstructionInBlock lastInstr (actBlockNum store)

addNewLLVMBlock :: Eval BlockNum
addNewLLVMBlock = do
    store <- get
    let next = (labelCounter store) + 1
    let newBlock = LLVMBlock {
        labelNum = next,
        instructions = [],
        lastInstr = Nothing
    }
    put $ store {
        blocks = Map.insert next newBlock (blocks store),
        actBlockNum = next,
        labelCounter = next
    }
    return next

debug :: Show a => a -> Eval ()
debug x = liftIO $ putStrLn $ show x

------------------- class utils -------------------------------------------

vtableIndex :: Integer
vtableIndex = 0

getSize :: Type -> Integer
getSize Str = getSize $ Ptr Char
getSize Int = 4
getSize Bool = 1
getSize Char = 1
getSize (Cls ident) = getSize $ Ptr $ Cls ident
getSize (Ptr _) = 8

getClassSize :: LatteClass -> Integer
getClassSize cls =
    let
        vptrSize = getSize $ Ptr $ VtableType (clsIdent cls)
        fieldsSize = Vector.sum $ Vector.map (getSize . getType) (fields cls)
    in 
        vptrSize + fieldsSize

getClass :: Ident -> Eval LatteClass
getClass ident = do
    store <- get
    let Just cls = Map.lookup ident (classes store)
    return cls

-- Assumes that field of given ident is declared in current class
getActClassField :: Ident -> Eval (Registry, Type)
getActClassField fieldIdent = do
    env <- ask
    let Just actCls = actClass env
    let Just index = Vector.findIndex (\f -> (getIdent f) == fieldIdent) (fields actCls)
    let field = (fields actCls) Vector.! index
    let ptrIndex = toInteger $ index + 1
    selfVal <- emitExpr (EVar selfIdent)
    let RegVal objReg = repr selfVal
    resultReg <- getNextRegistry
    addInstruction $ GetElementPtr resultReg (getType actCls) objReg ptrIndex
    return (resultReg, (getType field))

initFields :: LatteClass -> Registry -> Eval ()
initFields cls reg = 
    sequence_ $ map (initField reg) (zip [1..] (Vector.toList $ fields cls))
    where
        initField :: Registry -> (Integer, Field) -> Eval ()
        initField reg (index, (Field t _)) = do
            val <- defaultVal t
            ptr <- getNextRegistry
            addInstruction $ GetElementPtr ptr (Cls $ clsIdent cls) reg index
            addInstruction $ StoreInstr val ptr

castExpVal :: (Type, ExpVal) -> Eval ExpVal
castExpVal (actType, val) = case (actType, type_ val) of
    (Cls cls1, Cls cls2) -> 
        if cls1 /= cls2 then do
            resultReg <- getNextRegistry
            addInstruction $ Bitcast resultReg val actType
            vtableReg <- getNextRegistry
            let beforeCastType = Ptr $ VtableType cls2
            let afterCastType = Ptr $ VtableType cls1
            let vtableVal = ExpVal {repr = RegVal (vtableName cls2), type_ = beforeCastType}
            addInstruction $ Bitcast vtableReg vtableVal afterCastType
            let castedVtableVal = ExpVal {repr = RegVal vtableReg, type_ = afterCastType}
            setVtable resultReg actType castedVtableVal
            return ExpVal {repr = RegVal resultReg, type_ = actType}
        else
            return val
    _ -> return val

setVtable :: Registry -> Type -> ExpVal -> Eval ()
setVtable objReg objType vtableVal = do
    vtableReg <- getNextRegistry
    addInstruction $ GetElementPtr vtableReg objType objReg vtableIndex
    addInstruction $ StoreInstr vtableVal vtableReg

getVtableElem :: LatteClass -> Ident -> (Integer, VtableElem)
getVtableElem cls method =
    let
        Just index = Vector.findIndex (\elem -> (methodIdent elem) == method) (vtable cls)
    in
        (toInteger index, (vtable cls) Vector.! index)

-------------- expressions ------------------------------------

emitBinOpInstr :: Expr -> Expr -> BinOp -> Registry -> Eval Type
emitBinOpInstr e1 e2 operator resultReg = do
    val1 <- emitExpr e1
    val2 <- emitExpr e2
    addInstruction $ BinOpExpr resultReg operator val1 val2
    return Int

emitRelOpInstr :: Expr -> Expr -> RelOp -> Registry -> Eval Type
emitRelOpInstr e1 e2 relOp resultReg = do
    val1 <- emitExpr e1 
    val2 <- emitExpr e2
    addInstruction $ RelOpExpr resultReg relOp val1 val2
    return Bool

emitExprInstruction :: Expr -> Registry -> Eval Type
emitExprInstruction (EApp ident args) resultReg = do
    env <- ask
    let Just (EnvElem name t) = Map.lookup ident (funEnv env)
    argReprs <- mapM emitExpr args
    addInstruction $ Call resultReg t name argReprs
    return t
emitExprInstruction (Neg expr) resultReg =
    emitExprInstruction (EAdd (ELitInt 0) Minus expr) resultReg
emitExprInstruction (Not expr) resultReg = do
    val <- emitExpr expr
    addInstruction $ NotExpr resultReg val
    return Bool
emitExprInstruction (EMul expr1 Times expr2) resultReg =
    emitBinOpInstr expr1 expr2 Mul resultReg
emitExprInstruction (EMul expr1 Div expr2) resultReg =
    emitBinOpInstr expr1 expr2 DivOp resultReg 
emitExprInstruction (EMul expr1 Mod expr2) resultReg =
    emitBinOpInstr expr1 expr2 ModOp resultReg
-- Adding is handled separately in emitExpr
emitExprInstruction (EAdd expr1 Minus expr2) resultReg =
    emitBinOpInstr expr1 expr2 Sub resultReg
emitExprInstruction (ERel expr1 LTH expr2) resultReg =
    emitRelOpInstr expr1 expr2 LTH resultReg
emitExprInstruction (ERel expr1 LE expr2) resultReg =
    emitRelOpInstr expr1 expr2 LE resultReg 
emitExprInstruction (ERel expr1 GTH expr2) resultReg =
    emitRelOpInstr expr1 expr2 GTH resultReg
emitExprInstruction (ERel expr1 GE expr2) resultReg =
    emitRelOpInstr expr1 expr2 GE resultReg 
emitExprInstruction (ERel expr1 EQU expr2) resultReg =
    emitRelOpInstr expr1 expr2 EQU resultReg 
emitExprInstruction (ERel expr1 NE expr2) resultReg =
    emitRelOpInstr expr1 expr2 NE resultReg

getStringName :: String -> Eval Name
getStringName s = do
    store <- get
    case Map.lookup s (strConstants store) of
        Just name -> return name
        Nothing -> do
            let name = constName (constCounter store)
            put $ store {
                constCounter = (constCounter store) + 1,
                strConstants = Map.insert s name (strConstants store)
            }
            return name

getIdentReg :: Ident -> Eval (Registry, Type)
getIdentReg ident = do
    env <- ask
    case Map.lookup ident (varEnv env) of
        Nothing -> getActClassField ident
        Just (EnvElem reg t) -> return (reg, t)

emitExpr :: Expr -> Eval ExpVal
emitExpr (ELitInt n) = return $ numExpVal n
emitExpr ELitTrue = return $ boolExpVal True
emitExpr ELitFalse = return $ boolExpVal False
emitExpr (EString s) = do
    name <- getStringName s
    registry <- getNextRegistry
    addInstruction $ GetElementPtr registry (Ptr $ Arr (llvmStrLen s) Char) name 0
    return $ ExpVal {repr = RegVal registry, type_ = Str}
emitExpr (EAdd expr1 Plus expr2) = do
    val1 <- emitExpr expr1 
    val2 <- emitExpr expr2
    -- after type checking
    resultReg <- getNextRegistry
    case type_ val1 of
        Str -> do
            let concatFun = globalName $ Ident "concat_"
            addInstruction $ Call resultReg Str concatFun [val1, val2]
            return $ ExpVal {repr = RegVal resultReg, type_ = Str}
        Int -> do
            addInstruction $ BinOpExpr resultReg Add val1 val2
            return $ ExpVal {repr = RegVal resultReg, type_ = Int}
emitExpr (EAnd expr1 expr2) = do
    val1 <- emitExpr expr1
    actBlockNum1 <- getActBlockNum
    numTrue <- addNewLLVMBlock
    val2 <- emitExpr expr2
    actBlockNum2 <- getActBlockNum
    numNext <- addNewLLVMBlock
    setLastInstructionInBlock (CondJump val1 numTrue numNext) actBlockNum1
    setLastInstructionInBlock (Jump numNext) actBlockNum2
    resultReg <- getNextRegistry
    addInstruction $ Phi resultReg Bool [(falseExpVal, actBlockNum1), (val2, actBlockNum2)]
    return $ ExpVal {repr = RegVal resultReg, type_ = Bool}
emitExpr (EOr expr1 expr2) = do
    val1 <- emitExpr expr1
    actBlockNum1 <- getActBlockNum
    numFalse <- addNewLLVMBlock
    val2 <- emitExpr expr2
    actBlockNum2 <- getActBlockNum
    numNext <- addNewLLVMBlock
    setLastInstructionInBlock (CondJump val1 numNext numFalse) actBlockNum1
    setLastInstructionInBlock (Jump numNext) actBlockNum2
    resultReg <- getNextRegistry
    addInstruction $ Phi resultReg Bool [(trueExpVal, actBlockNum1), (val2, actBlockNum2)]
    return $ ExpVal {repr = RegVal resultReg, type_ = Bool}
emitExpr (EVar ident) = do
    (reg, t) <- getIdentReg ident
    resultReg <- getNextRegistry
    addInstruction $ Load resultReg t reg
    return $ ExpVal {repr = RegVal resultReg, type_ = t}
emitExpr (ENew ident) = do
    cls <- getClass ident
    let size = getClassSize cls
    mallocResult <- getNextRegistry
    addInstruction $ Call mallocResult (Ptr $ Char) (globalName $ Ident "malloc") [numExpVal size]
    let mallocVal = ExpVal {repr = RegVal mallocResult, type_ = Ptr Char}
    objReg <- getNextRegistry
    let pointerType = Cls ident
    addInstruction $ Bitcast objReg mallocVal pointerType
    let vtableVal = ExpVal {repr = RegVal (vtableName ident), type_ = (Ptr $ VtableType ident)}
    setVtable objReg pointerType vtableVal
    initFields cls objReg
    return ExpVal {repr = RegVal objReg, type_ = pointerType}
emitExpr (ENull ident) = return ExpVal {repr = NullVal, type_ = Cls ident}
emitExpr (EMApp objIdent methodId args) = do
    ExpVal (RegVal objReg) (Cls clsIdent) <- emitExpr (EVar objIdent)
    let vtableT = Ptr $ VtableType clsIdent
    vtablePtr <- getNextRegistry
    addInstruction $ GetElementPtr vtablePtr (Cls clsIdent) objReg vtableIndex
    vtableReg <- getNextRegistry
    addInstruction $ Load vtableReg vtableT vtablePtr
    cls <- getClass clsIdent
    let (index, vtableElem) = getVtableElem cls methodId
    funPointer <- getNextRegistry
    addInstruction $ GetElementPtr funPointer vtableT vtableReg index
    fun <- getNextRegistry
    let Ptr (Fun retType argTypes) = pointerType vtableElem
    addInstruction $ Load fun (pointerType vtableElem) funPointer
    argReprs <- mapM emitExpr args
    let self = ExpVal {repr = RegVal objReg, type_ = Cls clsIdent}
    castedArgs <- mapM castExpVal (zip argTypes (self:argReprs))
    resultReg <- getNextRegistry
    addInstruction $ Call resultReg retType fun castedArgs
    return ExpVal {repr = RegVal resultReg, type_ = retType}
emitExpr expr = do
    result <- getNextRegistry
    t <- emitExprInstruction expr result
    return $ ExpVal {repr = RegVal result, type_ = t}

----------- statements ------------------------------------------------

declare :: Ident -> Type -> ExpVal -> Eval Environment
declare ident actType val = do
    env <- ask
    reg <- getNextRegistry
    addInstruction $ Alloca reg actType
    castedVal <- castExpVal (actType, val)
    addInstruction $ StoreInstr castedVal reg
    return $ env {varEnv = Map.insert ident (EnvElem reg actType) (varEnv env)}

emitDeclarations :: Type -> [Item] -> Eval Environment
emitDeclarations _ [] = ask
emitDeclarations t (item:items) = case item of
    Init ident expr -> (do
        val <- emitExpr expr
        env <- declare ident t val
        local (\_ -> env) (emitDeclarations t items))
    NoInit ident -> do
        val <- defaultVal t
        env <- declare ident t val
        local (\_ -> env) (emitDeclarations t items)

emitCondExpr :: Expr -> BlockNum -> BlockNum -> BlockNum -> Eval ()
emitCondExpr (EAnd e1 e2) actBlock trueBlock falseBlock = do
    firstTrue <- addNewLLVMBlock
    emitCondExpr e1 actBlock firstTrue falseBlock
    emitCondExpr e2 firstTrue trueBlock falseBlock
emitCondExpr (EOr e1 e2) actBlock trueBlock falseBlock = do
    firstFalse <- addNewLLVMBlock
    emitCondExpr e1 actBlock trueBlock firstFalse
    emitCondExpr e2 firstFalse trueBlock falseBlock
emitCondExpr expr actBlock trueBlock falseBlock = do
    newestBlock <- getActBlockNum
    setActBlockNum actBlock
    val <- emitExpr expr
    setLastInstruction $ CondJump val trueBlock falseBlock 
    setActBlockNum newestBlock

emitStmt :: Stmt -> Eval Environment
emitStmt Empty = ask
emitStmt (BStmt block) = do
    emitBlock block
    ask
emitStmt (Ass ident expr) = do
    env <- ask
    val <- emitExpr expr
    (reg, actType) <- getIdentReg ident
    castedVal <- castExpVal (actType, val)
    addInstruction $ StoreInstr castedVal reg
    ask
emitStmt (Decl type_ items) = emitDeclarations type_ items
emitStmt (Incr ident) =
    emitStmt $ Ass ident (EAdd (EVar ident) Plus (ELitInt 1))
emitStmt (Decr ident) =
    emitStmt $ Ass ident (EAdd (EVar ident) Minus (ELitInt 1))
emitStmt (Ret expr) = do
    val <- emitExpr expr
    setLastInstruction $ ExpRet val
    ask
emitStmt VRet = do
    setLastInstruction VoidRet
    ask
emitStmt (Cond expr stmt) = do
    actBlockNum <- getActBlockNum
    trueBlockNum <- addNewLLVMBlock
    emitStmt stmt
    afterBlockNum <- addNewLLVMBlock
    emitCondExpr expr actBlockNum trueBlockNum afterBlockNum
    setLastIfNecessary trueBlockNum (Jump afterBlockNum) 
    ask
    where
        setLastIfNecessary :: BlockNum -> Instruction -> Eval ()
        setLastIfNecessary blockNum lastInstruction = do
            block <- getBlock blockNum
            case lastInstr block of
                Just _ -> return ()
                Nothing -> setLastInstructionInBlock lastInstruction blockNum
emitStmt (CondElse expr stmt1 stmt2) = do
    actBlockNum <- getActBlockNum
    trueBlockNum <- addNewLLVMBlock
    emitStmt stmt1
    actTrue <- getActBlock
    falseBlockNum <- addNewLLVMBlock
    emitStmt stmt2
    actFalse <- getActBlock
    emitCondExpr expr actBlockNum trueBlockNum falseBlockNum
    jumpToNewIfNecessary actTrue actFalse
    ask
    where
        jumpToNew :: LLVMBlock -> Eval ()
        jumpToNew block = do
            next <- addNewLLVMBlock
            setLastInstructionInBlock (Jump next) (labelNum block)
        jumpToNewIfNecessary :: LLVMBlock -> LLVMBlock -> Eval ()
        jumpToNewIfNecessary blockTrue blockFalse =
            case (lastInstr blockTrue, lastInstr blockFalse) of
                (Nothing, Nothing) -> do
                    after <- addNewLLVMBlock
                    setLastInstructionInBlock (Jump after) (labelNum blockTrue)
                    setLastInstructionInBlock (Jump after) (labelNum blockFalse)
                (Just _, Nothing) -> jumpToNew blockFalse
                (Nothing, Just _) -> jumpToNew blockTrue
                (Just _, Just _) -> return ()
emitStmt (While expr stmt) = do
    actBlockNum <- getActBlockNum
    addNewLLVMBlock
    emitStmt stmt
    loopBodyNum <- getActBlockNum
    loopCondNum <- addNewLLVMBlock
    setLastInstructionInBlock (Jump loopCondNum) actBlockNum
    setLastInstructionInBlock (Jump loopCondNum) loopBodyNum
    afterLoop <- addNewLLVMBlock
    emitCondExpr expr loopCondNum loopBodyNum afterLoop
    ask
emitStmt (SExp expr) = do 
    emitExpr expr
    ask

emitStmts :: [Stmt] -> Eval Environment
emitStmts [] = ask
emitStmts (stmt:stmts) = do
    env <- emitStmt stmt
    local (\_ -> env) (emitStmts stmts)

emitBlock :: Block -> Eval Environment
emitBlock (Block stmts) = do
    emitStmts stmts

---------------------- declarations, compiling instructions ---------------

addCompiled :: [String] -> Eval ()
addCompiled compiledInstructions =  do
    store <- get
    put $ store {compiled = compiledInstructions ++ (compiled store)}

reverseInstructions :: Eval ()
reverseInstructions = do
    store <- get
    put $ store {compiled = reverse (compiled store)}
 
compileBlock :: LLVMBlock -> [String]
compileBlock block = ["", formatInstr $ lastInstruction] ++ (map formatInstr (instructions block))
    where
        Just lastInstruction = lastInstr block
        formatInstr :: Instruction -> String
        formatInstr instr = printf "      %s" (show instr)

compileBlocksInstructions :: Eval ()
compileBlocksInstructions = do
    store <- get
    sequence_ $ map compileBlockWithLabel (map addMissingRet (Map.toList $ blocks store))
    store' <- get
    put $ store' {blocks = Map.empty}
    where
        -- return from void function
        addMissingRet :: (BlockNum, LLVMBlock) -> (BlockNum, LLVMBlock)
        addMissingRet (labelNum, block) = case (lastInstr block) of
            Just _ -> (labelNum, block)
            Nothing -> (labelNum, updated)
            where
                updated = LLVMBlock {
                    labelNum = labelNum,
                    instructions = instructions block,
                    lastInstr = Just VoidRet
                }
        compileBlockWithLabel :: (BlockNum, LLVMBlock) -> Eval ()
        compileBlockWithLabel (labelNum, block) = do
            addCompiled [formatLabel labelNum]
            addCompiled $ compileBlock block
        formatLabel :: BlockNum -> String
        formatLabel labelNum = printf "   %s:" (label labelNum)

emitFun :: FnDef -> Eval ()
emitFun (FnDef t ident args block) = do
    let funHeader = printf "define %s %s(%s) {" (showLLVMType t) (globalName ident) (showLLVMArgs args)
    addCompiled [funHeader]
    addNewLLVMBlock
    env <- declareArgs args
    local (\_ -> env) (emitBlock block)
    compileBlocksInstructions
    addCompiled ["", "}"]
    where
        declareArgs :: [Arg] -> Eval Environment
        declareArgs [] = ask
        declareArgs ((Arg actType ident):args) = do
            let reg = identReg ident
            let val = ExpVal {repr = RegVal reg, type_ = actType}
            env <- declare ident actType val
            local (\_ -> env) (declareArgs args)

createNewCls :: Ident -> [Field] -> Maybe LatteClass -> Eval LatteClass
createNewCls clsIdent fieldList ancestor = do
    let fieldsVector = Vector.fromList fieldList
    (clsVtable, clsFields) <- case ancestor of
        Nothing -> 
            return (Vector.empty, fieldsVector)
        Just ancestorCls -> 
            return ((vtable ancestorCls), ((fields ancestorCls) Vector.++ fieldsVector))
    let cls = LatteClass {
        clsIdent = clsIdent,
        fields = clsFields,
        ancestor = ancestor,
        vtable = clsVtable
    }
    return cls

updateCls :: Ident -> LatteClass -> Eval ()
updateCls ident cls = do
    store <- get
    put $ store {classes = Map.insert ident cls (classes store)}

emitMethod :: LatteClass -> FnDef -> Eval ()
emitMethod cls (FnDef retType ident args block) = do
    let name = methodName ident (clsIdent cls)
    local (\env -> env {actClass = Just cls}) (emitFun $ FnDef retType (Ident name) args block)

emitTopDef :: TopDef -> Eval ()
emitTopDef (FnTopDef fun) = emitFun fun
emitTopDef (ClsDef ident _ methods) = do
    cls <- getClass ident
    sequence_ $ map (emitMethod cls) methods
emitTopDef (ClsExtDef ident _ _ methods) = do
    cls <- getClass ident
    sequence_ $ map (emitMethod cls) methods

declareMethod :: FnDef -> LatteClass -> Eval LatteClass
declareMethod (FnDef retType ident args block) cls = do
    let name = globalName $ Ident $ methodName ident (clsIdent cls)
    let vtableElem = VtableElem {
        methodIdent = ident,
        pointerName = name,
        pointerType = Ptr (getType $ FnDef retType ident args block)
    }
    updatedVtable <- case Vector.findIndex (\elem -> (methodIdent elem) == ident) (vtable cls) of
        Nothing -> return $ Vector.snoc (vtable cls) (vtableElem)
        Just index -> return $ (vtable cls) Vector.// [(index, vtableElem)]
    return $ cls {vtable = updatedVtable}

declareMethods :: LatteClass -> [FnDef] -> Eval LatteClass
declareMethods cls [] = return cls
declareMethods cls (method:methods) = do
    updatedCls <- declareMethod method cls
    declareMethods updatedCls methods

createVtables :: TopDef -> Eval ()
createVtables (FnTopDef _) = return ()
createVtables (ClsDef ident fields methods) = do
    cls <- createNewCls ident fields Nothing
    updatedCls <- declareMethods cls methods
    updateCls (clsIdent updatedCls) updatedCls
createVtables (ClsExtDef ident ancestorIdent fields methods) = do
    ancestorCls <- getClass ancestorIdent
    cls <- createNewCls ident fields (Just ancestorCls)
    updatedCls <- declareMethods cls methods
    updateCls (clsIdent updatedCls) updatedCls

declareFunctions :: [TopDef] -> Eval Environment
declareFunctions [] = ask
declareFunctions ((FnTopDef (FnDef t ident _ _)):defs) = do
    let envElem = EnvElem (globalName ident) t
    local (\env -> env {funEnv = Map.insert ident envElem (funEnv env)}) (declareFunctions defs)
declareFunctions (_:defs) = declareFunctions defs

addClsDecl :: LatteClass -> Eval ()
addClsDecl cls = do
    let vtableTypeRepr = showVtableType (vtable cls) (clsIdent cls)
    let vtableRepr = showVtable (vtable cls) (clsIdent cls)
    let clsRepr = show cls
    addCompiled [vtableTypeRepr, vtableRepr, clsRepr]

addClassDeclarations :: Eval ()
addClassDeclarations = do
    store <- get
    sequence_ $ map addClsDecl (snd $ unzip $ Map.toList $ classes store)

addOuterDeclarations :: Eval ()
addOuterDeclarations = addCompiled $ map show declarations
    where
        declarations =
            [FunDecl "printInt" Void [Int],
            FunDecl "printString" Void [Str],
            FunDecl "error" Void [],
            FunDecl "readInt" Int [],
            FunDecl "readString" Str [],
            FunDecl "concat_" Str [Str, Str],
            FunDecl "malloc" (Ptr Char) [Int]]

addStringsDefinitions :: Eval ()
addStringsDefinitions = do
    store <- get
    addCompiled $ map getStringDecl (Map.toList (strConstants store))
    where
        getStringDecl :: (String, Name) -> String
        getStringDecl (s, name) =
            printf "%s = private constant [%s x i8] c\"%s\\00\"" name (show $ llvmStrLen s) (concat $ map llvmFormat s)
        llvmFormat :: Char -> String
        llvmFormat '\t' = "\\09"
        llvmFormat '\n' = "\\0A"
        llvmFormat '\\' = "\\5C"
        llvmFormat '\"' = "\\22"
        llvmFormat '\'' = "\\27"
        llvmFormat c = [c]

declareBuiltIn :: Eval Environment
declareBuiltIn =
    let
        builtIn = 
            [(Ident "printInt", EnvElem "@printInt" Void),
            (Ident "printString", EnvElem "@printString" Void),
            (Ident "error", EnvElem "@error" Void),
            (Ident "readInt", EnvElem "@readInt" Int),
            (Ident "readString", EnvElem "@readString" Str)]
    in 
        local (\env -> env {funEnv = Map.union (funEnv env) (Map.fromList builtIn)}) ask

emitProgram :: Program -> Eval ()
emitProgram (Program topDefs) = do
    env <- declareBuiltIn
    sequence_ $ map createVtables topDefs
    env' <- local (\_ -> env) (declareFunctions topDefs)
    local (\_ -> env') (sequence_ $ map emitTopDef topDefs)
    reverseInstructions
    addCompiled [""]
    addOuterDeclarations
    addCompiled [""]
    addClassDeclarations
    addCompiled [""]
    addStringsDefinitions
