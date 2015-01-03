module Backend where


import AbsLatte
import qualified Data.List as List
import qualified Data.Map as Map
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Text.Printf


type Name = String
type Registry = Name
type Counter = Integer
type Label = String
type LabelNum = Integer

data ExpValRepr =
    RegVal Name |
    NumVal Integer |
    BoolVal Bool |

instance Show ExpValRepr where
    show (RegVal name) = name
    show (NumVal n) = show n
    show (BoolVal True) = "true"
    show (BoolVal False) = "false"

data ExpVal = ExpVal {
    repr :: ExpValRepr,
    type_ :: Type
}

instance Show ExpVal where
    show val = printf "%s %s" (showLLVMType $ type_ val) (show $ repr val)

data BinOp = Add | Sub | Mul | Div | Mod

instance Show BinOp where
    show Add = "add"
    show Sub = "sub"
    show Mul = "mul"
    show Div = "sdiv"
    show Mod = "srem"

data Instruction =
    BinOpExpr Registry BinOp ExpVal ExpVal |
    RelOpExpr Registry RelOp ExpVal ExpVal |
    NotExpr Registry ExpVal |
    Load Registry Type Registry |
    Call Registry Type Name [ExpVal] |
    Store ExpVal Registry |
    Alloca Registry Type |
    VoidRet |
    ExpRet ExpVal |
    CondJump ExpVal LabelNum LabelNum |
    Jump LabelNum |
    Phi Registry Type [(ExpVal, LabelNum)]

instance Show RelOp where
    show LTH = "slt"
    show LE = "sle"
    show GTH = "sgt"
    show GE = "sge"
    show EQU = "eq"
    show NE = "ne"

instance Show Instruction where
    show (BinOpExpr result binop val1 val2) =
        let
            typeRepr = showLLVMType $ type_ val1
            val1Repr = show $ repr val1
            val2Repr = show $ repr val2
        in
            printf "%s = %s %s %s, %s" result (show binop_) typeRepr val1Repr val2Repr
    show (RelOpExpr result relop val1 val2) =
        let
            typeRepr = showLLVMType $ type_ val1
            val1Repr = show $ repr val1
            val2Repr = show $ repr val2
        in
            printf "%s = icmp %s %s %s, %s" result (show relop) typeRepr val1Repr val2Repr
    show (NotExpr result val) =
        printf "%s = xor i1 %s, true" result (show $ repr val)
    show (Load result type_ reg) = 
        printf "%s = load %s* %s" result (showLLVMType type_) reg
    show (Call result type_ fun args) = 
        printf "%s = call %s %s(%s)" result (showLLVMType type_) fun (showFunArgs args)
    show (Store val reg) =
        printf "store %s, %s* %s" (show val) (showLLVMType $ type_ val) reg
    show (Alloca reg type_) =
        printf "%s = alloca %s" reg (show type_)
    show VoidRet = "ret void"
    show (ExpRet val) = "ret %s" (show val)
    show (CondJump val labelNum1 labelNum2) =
        "br %s, label %%s, label %s" (show val) ('%':(label labelNum1)) (label labelNum2)
    show (Jump labelNum) = printf "br label %%s" (label labelNum)
    show (Phi result type_ exprsFromLabels) = 
        printf "%s = phi %s %s" result (show type_) (showPhiExprs exprsFromLabels)

data LLVMBlock = LLVMBlock {
    labelNum :: LabelNum,
    instructions :: [Instruction],
    lastInstr :: Maybe Instruction,
}

data EnvElem = EnvElem {
    name :: Name,
    type_ :: Type
}
type Env = Map.Map Ident EnvElem
data Environment = Environment {
    varEnv :: Env,
    funEnv :: Env
}
data Store = Store {
    blocks :: Map.Map LabelNum LLVMBlock,
    actBlockNum :: labelNum
    regCounter :: Counter,
    constCounter :: Counter,
    labelCounter :: Counter,
    strConstants :: Map.Map String Name
}

type Eval a = ReaderT Env (ErrorT String (StateT Store IO)) a


runEval :: Env -> Store -> Eval a -> IO (Either String a, Store)
runEval env state eval = runStateT (runErrorT (runReaderT eval env)) state

regName :: RegCounter -> Registry
regName regNum = "%r" ++ (show regNum)

globalName :: ident -> Name
globalName ident = '@':(name ident)

label :: LabelNum -> Label
label num = printf "label%s" (show num)

showFunArgs :: [ExpVal] -> String
showFunArgs args = unwords $ List.intersperse "," (map show args)

showPhiExprs :: [(ExpVal, LabelNum)] -> String
showPhiExprs exprsWithLabels = unwords $ List.intersperse "," (map show' exprsWithLabels)
    where
        show' :: (ExpVal, LabelNum) -> String
        show' (val, num) = printf "[ %s, %s ]" (show $ repr val) ('%':(label num))

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
        strConstants = strConstants store
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

changeBlock :: LabelNum -> (LLVMBlock -> LLVMBlock) -> Eval ()
changeBlock blockNum updateFun = do
    store <- get
    let Just actBlock = Map.lookup (blockNum store) (blocks store)
    put $ Store {
        blocks = Map.insert labelNum (updateFun actBlock),
        actBlockNum = actBlockNum store
        regCounter = regCounter store,
        constCounter = constCounter store,
        labelCounter = labelCounter store,
        strConstants = strConstants store,
    }

addInstructionsToBlock :: [Instruction] -> LabelNum -> Eval ()
addInstructionsToBlock instrs labelNum = changeBlock labelNum (addToBlock instrs)

addInstructions :: [Instruction] -> Eval ()
addInstructions instrs = do
    store <- get
    addInstructionsToBlock instrs (actBlockNum store)

addInstructionToBlock :: Instruction -> LabelNum -> Eval ()
addInstructionToBlock instr labelNum = addInstructionsToBlock [instr] labelNum

addInstruction :: Instruction -> Eval ()
addInstruction instr = addInstructions [instr]

setLastInstructionInBlock :: Instruction -> LabelNum -> Eval ()
setLastInstructionInBlock lastInstr labelNum = 
    changeBlock labelNum (setLastInBlock lastInstr)

setLastInstruction :: Instruction -> Eval ()
setLastInstruction lastInstr = do
    store <- get
    setLastInstructionInBlock (actBlockNum store) (setLastInBlock lastInstr)

emitBinOpInstruction :: Expr -> Expr -> BinOp -> Registry -> Eval (Type, Instruction)
emitBinOpInstruction e1 e2 Add resultReg = do
    val1 <- emitExprToBlock e1
    val2 <- emitExprToBlock e2
    -- after type checking
    case type_ val1 of
        Int -> return (Int, BinOpExpr resultReg Add val1 val2)
        Str -> return (Str, Concat resultReg (reg val1) (reg val2))
emitBinOpInstruction e1 e2 operator resultReg = do
    val1 <- emitExprToBlock e1
    val2 <- emitExprToBlock e2
    return (Int, BinOpExpr resultReg operator e1 e2)

emitRelOpInstruction :: Expr -> Expr -> RelOp -> Registry -> Eval (Type, Instruction)
emitRelOpInstruction e1 e2 RelOp resultReg = do
    val1 <- emitExprToBlock e1
    val2 <- emitExprToBlock e2
    return (Bool, RelOpExpr resultReg RelOp val1 val2)

-- TODO: useless
negate :: Expr -> Expr
negate ELitTrue = ELitFalse
negate ELitFalse = ELitTrue
negate (EAnd e1 e2) = EOr (negate e1) (negate e2)
negate (EOr e1 e2) = EAnd (negate e1) (negate e2)
negate (ERel e1 LTH e2) = ERel e1 GE e2
negate (ERel e1 LE e2) = ERel e1 GTH e2
negate (ERel e1 GTH e2) = ERel e1 LE e2
negate (ERel e1 EQU e2) = ERel e1 NE e2
negate (ERel e1 NE e2) = ERel e1 EQU e2

emitExprInstruction :: Expr -> Registry -> Eval (Type, Instruction)
emitExprInstruction (EVar ident) resultReg = do
    env <- ask
    let Just (EnvElem reg t) = Map.lookup ident (varEnv env)
    let instr = Load resultReg t reg
    return (t, instr)
emitExprInstruction (EApp ident args) resultReg = do
    env <- ask
    let Just (EnvElem name t) = Map.lookup ident (funEnv env)
    argReprs <- mapM emitExprToBlock args
    let instr = Call resultReg t name argReprs
    return (t, instr)
emitExprInstruction (Neg expr) resultReg =
    emitExprInstruction (EAdd (ELitInt 0) Minus expr) resultReg
emitExprInstruction (Not expr) resultReg = do
    val <- emitExprToBlock expr
    return (Bool, NotExpr resultReg val)
emitExprInstruction (EMul expr1 Times expr2) resultReg =
    emitBinOpInstruction expr1 expr2 Mul
emitExprInstruction (EMul expr1 Div expr2) resultReg =
    emitBinOpInstruction expr1 expr2 Div resultReg
emitExprInstruction (EMul expr1 Mod expr2) resultReg =
    emitBinOpInstruction expr1 expr2 Mod resultReg
emitExprInstruction (EAdd expr1 Plus expr2) resultReg =
    emitBinOpInstruction expr1 expr2 Add resultReg
emitExprInstruction (EAdd expr1 Minus expr2) resultReg =
    emitBinOpInstruction expr1 expr2 Sub resultReg
emitExprInstruction (ERel expr1 LTH expr2) resultReg =
    emitRelOpInstruction expr1 expr2 LTH resultReg
emitExprInstruction (ERel expr1 LE expr2) resultReg =
    emitRelOpInstruction expr1 expr2 LE resultReg
emitExprInstruction (ERel expr1 GTH expr2) resultReg =
    emitRelOpInstruction expr1 expr2 GTH resultReg
emitExprInstruction (ERel expr1 GE expr2) resultReg =
    emitRelOpInstruction expr1 expr2 GE resultReg
emitExprInstruction (ERel expr1 EQU expr2) resultReg =
    emitRelOpInstruction expr1 expr2 EQU resultReg
emitExprInstruction (ERel expr1 NE expr2) resultReg =
    emitRelOpInstruction expr1 expr2 NE resultReg

emitExprToBlock :: Expr -> LabelNum -> Eval ExpVal
emitExprToBlock (EAnd expr1 expr2) labelNum = do
    val1 <- emitExprToBlock expr1 labelNum
    numTrue <- addNewLLVMBlock
    val2 <- emitExprToBlock expr2 numTrue
    numNext <- addNewLLVMBlock
    addLastInstruction (CondJump val1 numTrue numNext) labelNum
    addLastInstruction (Jump val2 numNext) numTrue
    resultReg <- getNextRegistry
    addInstruction (Phi resultReg [(falseExpVal, labelNum), (val2, numTrue)]) numNext
    return $ ExpVal {repr = Reg resultReg, type_ = Bool}
emitExprToBlock (EOr expr1 expr2) labelNum = do
    val1 <- emitExprToBlock expr1 labelNum
    numFalse <- addNewLLVMBlock
    val2 <- emitExprToBlock expr2 numFalse
    numNext <- addNewLLVMBlock
    addLastInstruction (CondJump val1 numNext numFalse) labelNum
    addLastInstruction (Jump val2 numNext) numFalse
    resultReg <- getNextRegistry
    addInstruction (Phi resultReg [(trueExpVal, labelNum), (val2, numFalse)]) numNext
    return $ ExpVal {repr = Reg resultReg, type_ = Bool}
emitExprToBlock expr labelNum = do
    result <- getNextRegistry
    (t, instr) <- emitExprInstruction expr result
    addInstruction instr labelNum
    return $ ExpVal {repr = Reg result, type_ = t}


emitExpr :: Expr -> Eval ExprVal
emitExpr (ELitInt n) = return $ numExpVal n
emitExpr ELitTrue = return $ boolExpVal True
emitExpr ELitFalse = return $ boolExpVal False
emitExpr (EString string) = do
    addConstrString string
    reg <- convertString string
    return $ ExpVal (repr = Reg reg, type_ = Str}
emitExpr expr = do
    actBlock <- getActBlock
    emitExprToBlock expr actBlock


emitDeclarations :: Type -> [Item] -> [Instruction] -> Eval (Env, [Instruction])
emitDeclarations type_ [] accu = do
    env <- ask
    return (env, accu)
emitDeclarations type_ (item:items) accu = case item of
    Init ident expr -> (do
        val <- emitExprToBlock expr
        (env, updatedAccu) <- declareItem ident val accu
        local (\_ -> env) (emitDeclarations type_ items updatedAccu)
    NoInit ident -> do
        emptyStrReg <- getEmptyStrReg
        let val = (case type_ of
            Int -> numExpVal 0
            Bool -> falseExpVal
            Str -> ExpVal {repr = RegVal emptyStrReg, type_ = Str})
        (env, updatedAccu) <- declareItem ident val accu
        local (\_ -> env) (emitDeclarations type_ items updatedAccu)
    where
        declareItem :: Ident -> ExpVal -> [Instruction] -> Eval (Env, [Instruction])
        declareItem ident val accu = do
            env <- ask
            reg <- getNextRegistry
            let env' = Env {
                varEnv = Map.insert ident reg (varEnv env)
                procEnv = procEnv env
            }
            let alloca = Alloca reg (type_ val)
            let store = Store val reg
            return (env, (store:(alloca:accu))

emitCondExpr :: Expr -> LabelNum -> LabelNum -> LabelNum -> Eval ()
emitCondExpr (EAnd e1 e2) actBlock trueBlock falseBlock = do
    firstTrue <- addNewLLVMBlock
    emitCondExpr e1 actBlock firstTrue falseBlock
    emitCondExpr e2 firstTrue trueBlock falseBlock
emitCondExpr (EOr e1 e2) actBlock trueBlock falseBlock = do
    firstFalse <- addNewLLVMBlock
    emitCondExpr e1 actBlock trueBlock firstFalse
    emitCondExpr e2 firstFalse trueBlock falseBlock
emitCondExpr expr actBlock trueBlock falseBlock = do
    val <- emitExpr expr actBlock
    addLastInstruction (CondJump val trueBlock falseBlock) actBlock

emitStmt :: Stmt -> Eval Env
emitStmt Empty = return ()
emitStmt (BStmt block) = emitBlock block
emitStmt (Ass ident expr) = do
    env <- ask
    val <- emitExprToBlock expr
    Just reg t = Map.lookup ident (varEnv env)
    addInstruction $ Store val reg
    ask
emitStmt (Decl type_ items) = do
    (env, instructions) <- emitDeclarations type_ items []
    addInstructions instructions
    return env
emitStmt (Incr ident) =
    emitStmt $ EAss ident (EAdd (EVar ident) Plus (ELitInt 1))
emitStmt (Decr ident) =
    emitStmtInstr $ EAss ident (EAdd (EVar ident) Minus (ELitInt 1))
emitStmt (Ret expr) = do
    val <- emitExprToBlock
    addLastInstruction $ ExprRet val
    ask
emitStmt VRet = do
    addLastInstruction VoidRet
    ask
emitStmt (Cond expr stmt) = do
    actBlock <- getActBlock
    trueBlock <- addNewLLVMBlock
    emitStmt stmt
    afterBlock <- addNewLLVMBlock
    emitCondExpr actBlock trueBlock falseBlock
    ask
emitStmt (CondElse expr stmt1 stmt2) = do
    actBlock <- getActBlock
    trueBlock <- addNewLLVMBlock
    emitStmt stmt1
    falseBlock <- addNewLLVMBlock
    emitStmt stmt2
    emitCondExpr expr actBlock trueBlock falseBlock
    addLastInstr trueBlock
emitStmt (While expr stmt) = do
    actBlock <- getActBlock
    loop <- addNewLLVMBlock
    addLastInstruction (Jump loop)
    emitStmt stmt
    loopCond <- addNewLLVMBlock
    emitCondExpr loopCond loop 

emitStmts :: [Stmt] -> Eval Env
emitStmts [] = ask
emitStmts (stmt:stmts) = do
    env <- emitStmt stmt
    local (\_ -> env) (emitStmts stmts)

emitBlock :: Block -> Eval Env
emitBlock (Block stmts) = do
    emitStmts stmts
