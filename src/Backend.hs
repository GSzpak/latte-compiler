module Backend where


import AbsLatte
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Vector as Vector
import Control.Monad.Reader
import Control.Monad.State
import Text.Printf


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
    fields :: Vector.Vector Field
    ancestor :: Maybe LatteClass,
    vtableIdents :: Vtable
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
    classesReprs :: [String],
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

methodName :: Ident -> LatteClass -> Name
methodName methodId cls = printf "@%s.class.%s" (clsIdent cls) methodId

vtableName :: Ident -> Name
vtableName clsId = printf "@%s.class.vtable" clsId

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
showLLVMType (Cls ident) = classRepr ident
showLLVMType (Ptr t) = printf "%s*" (showLLVMType t)
showLLVMType (Arr len t) = printf "[%s x %s]" (show len) (showLLVMType t)
showLLVMType (Fun retType argTypes) =
    printf "%s (%s)" (showLLVMType retType) (map showLLVMType argTypes)
showLLVMType (VtableType ident) = printf "%%s.vtableType" ident 

showClsRepr :: LatteClass -> String
showClsRepr cls = classRepr (clsIdent cls)

printWithSeparator :: [String] -> String -> String
printWithSeparator strings sep = unwords $ List.intersperse sep strings

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

instance show LatteClass where
    show cls =
        let
            fieldsReprs = printWithSeparator (map showLLVMType (fieldTypes cls)) ","
        in
            case ancestor cls of
                Nothing -> printf "%s = type { %s }" (showClsRepr cls) fieldsReprs
                Just ancestorCls = printf "%s = type { %s , %s }" (showClsRepr cls) (showClsRepr ancestorCls) fieldsReprs

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

--------------------------------------------------------------------------

emptyEnv :: Environment
emptyEnv = Environment {
    varEnv = Map.empty,
    funEnv = Map.empty
}

emptyStore :: Store
emptyStore = Store {
    blocks = Map.empty,
    actBlockNum = 0,
    regCounter = 0,
    constCounter = 0,
    labelCounter = 0,
    strConstants = Map.empty,
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

getNextRegistry :: Eval Registry
getNextRegistry = do
    store <- get
    let actRegNum = regCounter store
    put $ Store {
        blocks = blocks store,
        actBlockNum = actBlockNum store,
        regCounter = actRegNum + 1,
        constCounter = constCounter store,
        labelCounter = labelCounter store,
        strConstants = strConstants store,
        compiled = compiled store
    }
    return $ regName actRegNum

addToBlock :: [Instruction] -> LLVMBlock -> LLVMBlock
addToBlock instrs block = LLVMBlock {
    labelNum = labelNum block,
    instructions = instrs ++ (instructions block),
    lastInstr = lastInstr block
}

setLastInBlock :: Instruction -> LLVMBlock -> LLVMBlock
setLastInBlock lastInstr block = LLVMBlock {
    labelNum = labelNum block,
    instructions = instructions block,
    lastInstr = Just lastInstr
}

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
    put $ Store {
        blocks = blocks store,
        actBlockNum = blockNum,
        regCounter = regCounter store,
        constCounter = constCounter store,
        labelCounter = labelCounter store,
        strConstants = strConstants store,
        compiled = compiled store
    }

changeBlock :: BlockNum -> (LLVMBlock -> LLVMBlock) -> Eval ()
changeBlock blockNum updateFun = do
    store <- get
    let Just actBlock = Map.lookup blockNum (blocks store)
    put $ Store {
        blocks = Map.insert blockNum (updateFun actBlock) (blocks store),
        actBlockNum = actBlockNum store,
        regCounter = regCounter store,
        constCounter = constCounter store,
        labelCounter = labelCounter store,
        strConstants = strConstants store,
        compiled = compiled store
    }

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
    put $ Store {
        blocks = Map.insert next newBlock (blocks store),
        actBlockNum = next,
        regCounter = regCounter store,
        constCounter = constCounter store,
        labelCounter = next,
        strConstants = strConstants store,
        compiled = compiled store
    }
    return next

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
            put $ Store {
                blocks = blocks store,
                actBlockNum = actBlockNum store,
                regCounter = regCounter store,
                constCounter = (constCounter store) + 1,
                labelCounter = labelCounter store,
                strConstants = Map.insert s name (strConstants store),
                compiled = compiled store
            }
            return name

getClass :: Ident -> Eval LatteClass
getClass ident = do
    store <- get
    let Just cls = Map.lookup ident (classes store)
    return cls

getClassWithField :: Ident -> LatteClass -> Eval (LatteClass, Integer)
getField fieldIdent cls = do
    case findIndex (\f -> (getIdent f) == fieldIdent) (fields cls) of
        Just index -> return (cls, toInteger index)
        Nothing -> do
            -- Program was accepted by frontend, therefore field must be inherited
            let Just ancestorCls = ancestor cls
            getClassWithField fieldIdent ancestorCls

thisIdent :: Ident
thisIdent = Ident ".this"

emitGetField :: LatteClass -> Integer -> Registry -> Type -> Eval ExpVal
emitGetField cls index objReg objType = do
    let field = (fields cls) ! index
    ptrIndex <- case ancestor cls of
        Nothing -> return $ toInteger $ index + 1
        Just _ -> return $ toInteger $ index + 2
    resultReg <- getNextRegistry
    addInstruction $ GetElementPtr resultReg clsType objReg ptrIndex
    return $ ExpVal {repr = RegVal resultReg, type_ = (getType field)}

getActClassField :: Ident -> Eval ExpVal
getActClassField fieldIdent = do
    env <- ask
    let Just actCls = actClass env
    (clsWithField, index) <- getClassWithField fieldIdent actCls
    thisVal <- emitExpr (EVar thisIdent)
    if (clsIdent actCls) /= (clsIdent clsWithField) then do
        bitcastReg <- getNextRegistry
        let clsType = Ptr $ Cls $ clsIdent clsWithField
        addInstruction $ Bitcast bitcastReg thisVal clsType
        emitGetField clsWithField index bitcastReg clsType
    else
        let RegVal reg = repr thisVal
        emitGetField clsWithField index reg (type_ thisVal)

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
    env <- ask
    case Map.lookup ident (varEnv env) of
        Nothing -> getActClassField ident
        Just (EnvElem reg t) -> do
            resultReg <- getNextRegistry
            addInstruction $ Load resultReg t reg
            return $ ExpVal {repr = RegVal resultReg, type_ = t}
emitExpr (ENew ident) = do
    cls <- getClass ident
    let size = getSize cls
    mallocResult <- getNextRegistry
    addInstruction $ Call mallocResult (Ptr $ Char) (globalName $ Ident "malloc") [numExpVal size]
    let mallocVal = ExpVal {repr = RegVal mallocResult, type_ = Ptr Char}
    resultReg <- getNextRegistry
    let pointerType = Ptr $ Cls ident
    addInstruction $ Bitcast resultReg mallocVal pointerType
    setVtable (clsIdent cls) pointerType resultReg
    return ExpVal {repr = RegVal resultReg, type_ = pointerType}
emitExpr (ENull ident) = return ExpVal {repr = NullVal, type_ = Ptr $ Cls ident}
emitExpr (EMapp ident methodId args) = do
    objVal <- emitExpr (EVar ident)
    let RegVal reg = repr objVal
    let Ptr (Cls clsIdent) = type_ objVal
    let vtableT = Ptr $ VtableType clsIdent
    vtablePtr <- getNextRegistry
    addInstruction $ GetElementPtr vtablePtr (type_ objVal) reg vtableIndex
    vtableReg <- getNextRegistry
    addInstruction $ Load vtableReg vtableT vtablePtr
    cls <- getClass clsIdent
    let (index, vtableElem) = getVTableElem cls methodId
    funPointer <- getNextRegistry
    addInstruction $ GetElementPtr funPointer vtableT vtableReg index
    fun <- getNextRegistry
    let Ptr (Fun retType argTypes) = pointerType vtableElem
    addInstruction $ Load fun (pointerType vtableElem) funPointer
    argReprs <- mapM emitExpr args
    resultReg <- getNextRegistry
    addInstruction $ Call resultReg retType fun argReprs
    return ExpVal {repr = RegVal resultReg, type_ = retType}
emitExpr expr = do
    result <- getNextRegistry
    t <- emitExprInstruction expr result
    return $ ExpVal {repr = RegVal result, type_ = t}

setVtable :: Ident -> Type -> Registry -> Eval ()
setVtable clsIdent pointerType pointerReg = do
    vtableReg <- getNextRegistry
    addInstruction $ GetElementPtr vtableReg pointerType pointerReg vtableIndex
    let vtableVal = ExpVal {repr = RegVal (vtableName clsIdent), type_ = (Ptr $ VtableType clsIdent)}
    addInstruction $ Store vtableVal vtableReg

getVtableElem :: LatteClass -> Ident -> (Integer, VtableElem)
getVTableElem cls method =
    let
        Just index = findIndex (\elem -> (methodIdent elem) == method) (vtable cls)
    in
        (toInteger index, (vtable cls) ! index)

vtableIndex :: Integer
vtableIndex = 0

addrSize :: Integer
addrSize = 8

-- TODO: move to utils
getSize :: LatteClass -> Integer
getSize cls = case (ancestor cls) of
    Nothing -> addrSize * ((length $ fields cls) + 1)
    Just _ -> addrSize * ((length $ fields cls) + 2)

----------- statements ------------------------------------------------

setVtable :: ExpVal -> Eval ()
setVtable val = do
    let Ptr (Cls clsId) = type_ val
    let RegVal reg = repr val


declare :: Ident -> Type -> ExpVal -> Eval Environment
declare ident actType val = do
    env <- ask
    reg <- getNextRegistry
    let env' = Environment {
        varEnv = Map.insert ident (EnvElem reg (type_ val)) (varEnv env),
        funEnv = funEnv env
    }
    let alloca = Alloca reg t
    let store = StoreInstr val reg
    addInstructions [store, alloca]
    return env'

emitStoreInstr :: Type -> Registry -> ExpVal -> Eval ()
emitStoreInstr actType result val =
    if actType /= type_ val then do
        bitcastReg <- getNextRegistry
        addInstruction $ Bitcast bitcastReg val actType 
        let newVal = ExpVal {repr = RegVal bitcastReg, type_ = actType}
        addInstruction $ StoreInstr newVal result
    else
        addInstruction $ StoreInstr val result
emitDeclarations :: Type -> [Item] -> Eval Environment
emitDeclarations _ [] = ask
emitDeclarations t (item:items) = case item of
    Init ident expr -> (do
        val <- emitExpr expr
        env <- declare ident t val
        local (\_ -> env) (emitDeclarations t items))
    NoInit ident -> do
        val <- case t of
            Int -> return $ numExpVal 0
            Bool -> return falseExpVal
            Str -> emitExpr $ EString ""
            _ -> return NullVal
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

updateAssignment :: Ident -> Registry -> Type -> ExpVal -> Eval Environment
updateAssignment ident valReg actType@(Ptr $ Cls oldCls) newVal = do
    if actType == type_ newVal then
        ask
    else do
        let RegVal reg = repr newVal
        let Ptr (Cls newCls) = type_ newVal
        result <- 
        addInstruction $ Bitcast 
updateAssignment _ _ _ _ = ask

emitStmt :: Stmt -> Eval Environment
emitStmt Empty = ask
emitStmt (BStmt block) = do
    emitBlock block
    ask
emitStmt (Ass ident expr) = do
    env <- ask
    val <- emitExpr expr
    let Just (EnvElem reg t) = Map.lookup ident (varEnv env)
    env' <- updateAssignment ident reg t val
    addInstruction $ StoreInstr val reg
    if t /= type_ val then
        return $ Environment {
            varEnv = Map.insert ident (EnvElem reg (type_ val)) (varEnv env)
            funEnv = funEnv env,
            actClass = actClass env
        }
    else
        return env
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

addCompiled :: [String] -> Eval ()
addCompiled compiledInstructions =  do
    store <- get
    put $ Store {
        blocks = blocks store,
        actBlockNum = actBlockNum store,
        regCounter = regCounter store,
        constCounter = constCounter store,
        labelCounter = labelCounter store,
        strConstants = strConstants store,
        compiled = compiledInstructions ++ (compiled store)
    }

showLLVMArgs :: [Arg] -> String
showLLVMArgs args = printWithSeparator (map showLLVMArg args) ","
    where
        showLLVMArg :: Arg -> String
        showLLVMArg (Arg type_ ident) = printf "%s %s" (showLLVMType type_) (identReg ident)

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
    put $ Store {
        blocks = Map.empty,
        actBlockNum = actBlockNum store',
        regCounter = regCounter store',
        constCounter = constCounter store',
        labelCounter = labelCounter store',
        strConstants = strConstants store',
        compiled = compiled store'
    }
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

addArgs :: [Arg] -> Eval Environment
addArgs [] = ask
addArgs ((Arg type_ ident):args) = do
    local addArg (addArgs args)
    where
        addArg :: Environment -> Environment
        addArg env = Environment {
            varEnv = Map.insert ident (EnvElem (identReg ident) type_) (varEnv env),
            funEnv = funEnv env
        }

createNewCls :: Ident -> Fields -> Maybe LatteClass -> Eval LatteClass
createNewCls clsIdent fields ancestor = do
    clsVtable <- case ancestor of
        Nothing -> return Vector.empty
        Just ancestorCls -> return (vtable ancestorCls)
    let cls = LatteClass {
        clsIdent = clsIdent,
        fields = Vector.fromList fields,
        ancestor = ancestor,
        vtable = clsVtable
    }
    return cls

updateCls :: Ident -> LatteClass -> Eval ()
updateCls ident cls = do
    store <- get
    put $ Store {
        blocks = blocks store,
        actBlockNum = actBlockNum store,
        regCounter = regCounter store,
        constCounter = constCounter store,
        labelCounter = labelCounter store,
        strConstants = strConstants store,
        classes = Map.insert ident cls (classes store),
        compiled = compiled store
    }

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
        declareArgs ((Arg t ident):args) = do
            let reg = identReg ident
            let val = ExpVal {repr = RegVal reg, type_ = t}
            env <- declare ident val
            local (\_ -> env) (declareArgs args)

emitMethod :: LatteClass -> FnDef -> Eval ()
emitMethod cls (FnDef retType ident args block) = do
    let thisArg = Arg (Ptr $ Cls $ clsIdent cls) thisIdent
    let name = methodName ident cls
    local (updateActCls cls) (emitFun $ FnDef retType name (thisArg:args) block)
    where
        updateActCls :: LatteClass -> Environment -> Environment
        updateActCls cls env = Environment {
            varEnv = varEnv env,
            funEnv = funEnv env,
            actClass = Just cls
        }

emitTopDef :: TopDef -> Eval ()
emitTopDef (FnTopDef fun) = emitFun fun
emitTopDef (ClsDef ident _ methods) = do
    cls <- getClass ident
    sequence_ $ map (emitMethod cls) methods
emitTopDef (ClsExtDef ident _ _ methods) = do
    cls <- getClass ident
    sequence_ $ map (emitMethod cls) methods

declareMethod :: FnDef -> LatteClass -> Eval LatteClass
declareMethod fun@(FnDef _ ident _ _) cls = do
    let name = methodName ident cls
    let vtableElem = VtableElem {
        methodIdent = ident,
        pointerName = name,
        pointerType = Ptr (getType fun)
    }
    updatedVtable <- case findIndex (\elem -> (methodIdent elem) == ident) (vtable cls) of
        Nothing -> snoc (vtable cls) (vtableElem)
        Just index -> (vtable cls) // [(index, vtableElem)]
    return $ LatteClass {
        clsIdent = clsIdent cls,
        fields = fields cls,
        ancestor = ancestor cls,
        vtable = updatedVtable
    }

reverseInstructions :: Eval ()
reverseInstructions = do
    store <- get
    put $ Store {
        blocks = blocks store,
        actBlockNum = actBlockNum store,
        regCounter = regCounter store,
        constCounter = constCounter store,
        labelCounter = labelCounter store,
        strConstants = strConstants store,
        compiled = reverse (compiled store)
    }
 
addOuterDeclarations :: Eval ()
addOuterDeclarations = addCompiled $ map show declarations
    where
        declarations =
            [FunDecl "printInt" Void [Int],
            FunDecl "printString" Void [Str],
            FunDecl "error" Void [],
            FunDecl "readInt" Int [],
            FunDecl "readString" Str [],
            FunDecl "concat_" Str [Str, Str]]

addClassDeclarations :: Eval ()
addClassDeclarations = do
    store <- get
    put $ Store {
        blocks = blocks store,
        actBlockNum = actBlockNum store,
        regCounter = regCounter store,
        constCounter = constCounter store,
        labelCounter = labelCounter store,
        strConstants = strConstants store,
        classes = classes Store,
        classesReprs = [],
        compiled = (classesReprs store) ++ (compiled store)
    }
    

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

declareMethods :: LatteClass -> [FnDef] -> Eval LatteClass
declareMethods cls [] = return cls
declareMethods cls (method:methods) = do
    updatedCls <- declareMethod method cls
    declareMethods updatedCls methods

-- TODO: move classes to env
createVtables :: TopDef -> Eval ()
createVtables (FnTopDef _) = return ()
createVtables (ClsDef ident fields methods) = do
    cls <- addNewCls ident fields Nothing
    updatedCls <- declareMethods cls methods
    updateCls (clsIdent updatedCls) updatedCls
createVtables (ClsExtDef ident ancestorIdent fields methods) = do
    ancestorCls <- getClass ancestorIdent
    cls <- addNewCls ident fields (Just ancestorIdent)
    updatedCls <- declareMethods cls methods
    updateCls (clsIdent updatedCls) updatedCls

declareFunctions :: [TopDef] -> Eval Environment
declareFunctions [] = ask
declareFunctions ((FnTopDef (FnDef t ident _ _)):defs) = do
    local addFun (declareFunctions defs)
    where
        addFun :: Environment -> Environment
        addFun env = Environment {
            varEnv = varEnv env,
            funEnv = Map.insert ident (EnvElem (globalName ident) t) (funEnv env)
        }

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
        local (addFunList builtIn) ask
    where
        addFunList :: [(Ident, EnvElem)] -> Environment -> Environment
        addFunList funList env = Environment {
            varEnv = varEnv env,
            funEnv = Map.union (funEnv env) (Map.fromList funList)
        }

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
