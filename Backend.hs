module Backend where


import AbsLatte
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
    show (BoolVal b) = show b

data ExpVal = ExpVal {
    repr :: ExpValRepr,
    type_ :: Type
}
data BinOp = Add | Sub | Mul | Div | Mod
data Instruction =
    BinOpExpr Registry BinOp ExpVal ExpVal |
    RelOpExpr Registry RelOp ExpVal ExpVal |
    NotExpr Registry ExpVal |
    Concat Registry Registry Registry |
    Load Registry Type Registry |
    Call Registry Type Name [ExpVal] |
    Store ExpVal Registry |
    Alloca Registry Type |
    VoidRet |
    ExpRet ExpVal |
    CondJump ExpVal LabelNum LabelNum |
    Jump LabelNum |
    Phi Registry [ExpVal, LabelNum]
    
data LLVMBlock = LLVMBlock {
    label :: Label,
    instructions :: [Instruction],
    predecessors :: [LabelNum],
    successors :: [LabelNum]
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
    actInstructions :: [LLVmInstr],
    regCounter :: Counter,
    constCounter :: Counter,
    labelCounter :: Counter,
    strConstants :: Map.Map String Name
}

type Eval a = ReaderT Env (ErrorT String (StateT Store IO)) a

runEval :: Env -> Store -> Eval a -> IO (Either String a, Store)
runEval env state eval = runStateT (runErrorT (runReaderT eval env)) state

getRegistry :: RegCounter -> Registry
getRegistry regNum = "%r" ++ (show regNum)

globalName :: ident -> Name
globalName ident = '@':(name ident)

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
        blocks = blocks store
        actInstructions = actInstructions store
        regCounter = actRegNum + 1
        constCounter = constCounter store
        labelCounter = labelCounter store
        strConstants = strConstants store
    }
    return $ getRegistry actRegNum

addInstructions :: [Instruction] -> Eval ()
addInstructions instrs = do
    store <- get
    put $ Store {
        blocks = blocks store
        actInstructions = instrs ++ (actInstructions store)
        regCounter = actRegNum + 1
        constCounter = constCounter store
        labelCounter = labelCounter store
        strConstants = strConstants store
    }

addInstruction :: Instruction -> Eval ()
addInstruction instr = addInstructions [instr]

emitBinOpInstruction :: Expr -> Expr -> BinOp -> Registry -> Eval (Type, Instruction)
emitBinOpInstruction e1 e2 Add resultReg = do
    val1 <- emitExpr e1
    val2 <- emitExpr e2
    -- after type checking
    case type_ val1 of
        Int -> return (Int, BinOpExpr resultReg Add val1 val2)
        Str -> return (Str, Concat resultReg (reg val1) (reg val2))
emitBinOpInstruction e1 e2 operator resultReg = do
    val1 <- emitExpr e1
    val2 <- emitExpr e2
    return (Int, BinOpExpr resultReg operator e1 e2)

emitRelOpInstruction :: Expr -> Expr -> RelOp -> Registry -> Eval (Type, Instruction)
emitRelOpInstruction e1 e2 RelOp resultReg = do
    val1 <- emitExpr e1
    val2 <- emitExpr e2
    return (Bool, RelOpExpr resultReg RelOp val1 val2)

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
    argReprs <- mapM emitExpr args
    let instr = Call resultReg t name argReprs
    return (t, instr)
emitExprInstruction (Neg expr) resultReg =
    emitExprInstruction (EAdd (ELitInt 0) Minus expr) resultReg
emitExprInstruction (Not expr) resultReg = do
    val <- emitExpr expr
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

emitExpr :: Expr -> LabelNum -> Eval ExpVal
emitExpr (ELitInt n) _ = return $ numExpVal n
emitExpr ELitTrue _ = return $ boolExpVal True
emitExpr ELitFalse _ = return $ boolExpVal False
-- TODO: EString
emitExpr (EAnd expr1 expr2) labelNum = do
    val1 <- emitExpr expr1 labelNum
    numTrue <- addNewLLVMBlock
    val2 <- emitExpr expr2 numTrue
    numNext <- addNewLLVMBlock
    addInstruction (CondJump val1 numTrue numNext) labelNum
    addInstruction (Jump val2 numNext) numTrue
    resultReg <- getNextRegistry
    addInstruction (Phi resultReg [(falseExpVal, labelNum), (val2, numTrue)]) numNext
    return $ ExpVal {repr = Reg resultReg, type_ = Bool}
emitExpr (EOr expr1 expr2) labelNum = do
    val1 <- emitExpr expr1 labelNum
    numFalse <- addNewLLVMBlock
    val2 <- emitExpr expr2 numFalse
    numNext <- addNewLLVMBlock
    addInstruction (CondJump val1 numNext numFalse) labelNum
    addInstruction (Jump val2 numNext) numFalse
    resultReg <- getNextRegistry
    addInstruction (Phi resultReg [(trueExpVal, labelNum), (val2, numFalse)]) numNext
    return $ ExpVal {repr = Reg resultReg, type_ = Bool}
emitExpr expr labelNum = do
    result <- getNextRegistry
    (t, instr) <- emitExprInstruction expr result
    addInstruction instr labelNum
    return $ ExpVal {repr = Reg result, type_ = t}

emitDeclarations :: Type -> [Item] -> [LLVmInstr] -> Eval (Env, [LLVmInstr])
emitDeclarations type_ [] accu = do
    env <- ask
    return (env, accu)
emitDeclarations type_ (item:items) accu = case item of
    Init ident expr -> (do
        val <- emitExpr expr
        (env, updatedAccu) <- declareItem ident val accu
        local (\_ -> env) (emitDeclarations type_ items updatedAccu)
    NoInit ident -> do
        emptyStrReg <- getEmptyStrReg
        let val = (case type_ of
            Int -> ExpVal {repr = NumVal 0, type_ = Int}
            Bool -> ExpVal {repr = BoolVal False, type_ = Bool}
            Str -> ExpVal {repr = RegVal emptyStrReg, type_ = Str})
        (env, updatedAccu) <- declareItem ident val accu
        local (\_ -> env) (emitDeclarations type_ items updatedAccu)
    where
        declareItem :: Ident -> ExpVal -> [LLVmInstr] -> Eval (Env, [LLVmInstr])
        declareItem ident val accu = do
            env <- ask
            reg <- getNextRegistry
            let env' = Env {
                varEnv = Map.insert ident reg (varEnv env)
                procEnv = procEnv env
            }
            let alloca = WithResult reg (Alloca (type_ val))
            let store = WithoutResult $ Store val reg
            return (env, (store:(alloca:accu))

emitStmt :: Stmt -> Eval Env
emitStmt Empty = return ()
emitStmt (BStmt block) = emitBlock block
emitStmt (Ass ident expr) = do
    env <- ask
    val <- emitExpr expr
    Just reg t = Map.lookup ident (varEnv env)
    addInstruction $ WithoutResult $ Store val reg
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
    val <- emitExpr
    addInstruction $ WithoutResult $ ExprRet val
    ask
emitStmt VRet = do
    addInstruction $ WithoutResult VoidRet
    ask
emitStmt (Cond expr stmt) = do
    val <- emitExpr expr


emitStmts :: [Stmt] -> Eval Env
emitStmts [] = ask
emitStmts (stmt:stmts) = do
    env <- emitStmt stmt
    local (\_ -> env) (emitStmts stmts)

emitBlock :: Block -> Eval Env
emitBlock (Block stmts) = do
    addNewLLVMBlock
    emitStmts stmts
