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

data ExpValRepr =
    RegVal Name |
    NumVal Integer |
    BoolVal Bool |
    StrVal String
instance Show ExpValRepr where
    show (RegVal name) = name
    show (NumVal n) = show n
    show (BoolVal b) = show b
    show (StrVal s) = s

data ExpVal = ExpVal {
    repr :: ExpValRepr,
    type_ :: Type
}
data BinOp = Add | Sub | Mul | Div | Mod
data Instruction =
    BinOpExpr BinOp ExpVal ExpVal |
    RelOpExpr RelOp ExpVal ExpVal |
    Concat Registry Registry |
    Load Type Registry |
    Call Type Name [ExpVal] |
    Store ExpVal Registry |
    Alloca Type
    
data LLVmInstr = 
    WithResult Registry Instruction |
    WithoutResult Instruction

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
    revInstructions :: [LLVmInstr],
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

getNextRegistry :: Eval Registry
getNextRegistry = do
    store <- get
    let actRegNum = regCounter store
    put $ Store {
        revInstructions = revInstructions store
        regCounter = actRegNum + 1
    }
    return $ getRegistry actRegNum

addInstructions :: [LLVmInstr] -> Eval ()
addInstructions instrs = do
    store <- get
    put $ Store {
        revInstructions = instrs ++ (revInstructions store)
        regCounter = regCounter store
    }

addInstruction :: LLVmInstr -> Eval ()
addInstruction instr = addInstructions [instr]

emitBinOpInstruction :: Expr -> Expr -> BinOp -> Eval (Type, Instruction)
emitBinOpInstruction e1 e2 Add = do
    val1 <- emitExpr e1
    val2 <- emitExpr e2
    -- after type checking
    case type_ val1 of
        Int -> return (Int, BinOpExpr Add val1 val2)
        Str -> return (Str, Concat (reg val1) (reg val2))
emitBinOpInstruction e1 e2 operator = do
    val1 <- emitExpr e1
    val2 <- emitExpr e2
    return (Int, BinOpExpr operator e1 e2)

emitRelOpInstruction :: Expr -> Expr -> RelOp -> Eval (Type, Instruction)
emitRelOpInstruction e1 e2 RelOp = do
    val1 <- emitExpr e1
    val2 <- emitExpr e2
    return (Bool, RelOpExpr RelOp val1 val2)

emitExprInstruction :: Expr -> Eval (Type, Instruction)
emitExprInstruction (EVar ident) = do
    env <- ask
    let Just (EnvElem reg t) = Map.lookup ident (varEnv env)
    let instr = Load t reg
    return (t, instr)
emitExprInstruction (EApp ident args) = do
    env <- ask
    let Just (EnvElem name t) = Map.lookup ident (funEnv env)
    argReprs <- mapM emitExpr args
    let instr = Call t name argReprs
    return (t, instr)
emitExprInstruction (Neg expr) = emitExprInstruction (EAdd (ELitInt 0) Minus expr)
-- TODO: not
emitExprInstruction (EMul expr1 Times expr2) = emitBinOpInstruction expr1 expr2 Mul
emitExprInstruction (EMul expr1 Div expr2) = emitBinOpInstruction expr1 expr2 Div
emitExprInstruction (EMul expr1 Mod expr2) = emitBinOpInstruction expr1 expr2 Mod
emitExprInstruction (EAdd expr1 Plus expr2) = emitBinOpInstruction expr1 expr2 Add
emitExprInstruction (EAdd expr1 Minus expr2) = emitBinOpInstruction expr1 expr2 Sub
emitExprInstruction (ERel expr1 LTH expr2) = emitRelOpInstruction expr1 expr2 LTH
emitExprInstruction (ERel expr1 LE expr2) = emitRelOpInstruction expr1 expr2 LE
emitExprInstruction (ERel expr1 GTH expr2) = emitRelOpInstruction expr1 expr2 GTH
emitExprInstruction (ERel expr1 GE expr2) = emitRelOpInstruction expr1 expr2 GE
emitExprInstruction (ERel expr1 EQU expr2) = emitRelOpInstruction expr1 expr2 EQU
emitExprInstruction (ERel expr1 NE expr2) = emitRelOpInstruction expr1 expr2 NE
-- TODO: and, or
emitExprInstruction (EAnd expr1 expr2)

emitExpr :: Expr -> Eval ExpVal
emitExpr (ELitInt n) = return $ ExpVal {repr = NumVal n, type_ = Int}
emitExpr ELitTrue = return $ ExpVal {repr = BoolVal True, type_ = Bool}
emitExpr ELitFalse = return $ ExpVal {repr = BoolVal False, type_ = Bool}
-- TODO: EString
emitExpr expr = do
    result <- getNextRegistry
    (t, instr) <- emitExprInstruction expr
    addInstruction $ WithResult result instr
    return $ ExpVal {repr = Reg result, type_ = t}

emitStmtInstr :: Stmt -> Eval (Env, [LLVmInstr])
emitStmtInstr (Ass ident expr) = do
    env <- ask
    val <- emitExpr expr
    Just reg t = Map.lookup ident (varEnv env)
    let instr = Store val reg
    return $ (env, [WithoutResult instr])
emitStmtInstr (Decl type_ items) = emitDeclarations items []
    where
        declareItem :: Ident -> ExpVal -> [LLVmInstr] -> Eval (Env, [LLVmInstr])
        declareItem ident val accu = do
            env <- ask
            reg <- getNextRegistry
            let env' = Env {
                varEnv = Map.insert ident reg (varEnv env)
                procEnv = procEnv env
            }
            let alloca = WithResult reg (Alloca type_)
            let store = WithoutResult $ Store val reg
            return (env, (store:(alloca:accu))
        
        declareItems :: Ident -> ExpVal -> [Item] -> [LLVmInstr] -> Eval (Env, [LLVmInstr])
        declareItems ident val items accu = do
            (env, updatedAccu) <- declareItem ident val accu
            local (\_ -> env) (emitDeclarations items updatedAccu)
        
        emitDeclarations :: [Item] -> [LLVmInstr] -> Eval (Env, [LLVmInstr])
        emitDeclarations [] accu = return accu
        emitDeclarations (item:items) = case item of
            Init ident expr -> (do
                val <- emitExpr expr
                declareItems ident val items accu
            NoInit ident -> do
                emptyStrReg <- getEmptyStrReg
                let val = (case type_ of
                    Int -> ExpVal {repr = NumVal 0, type_ = Int}
                    Bool -> ExpVal {repr = BoolVal False, type_ = Bool}
                    Str -> ExpVal {repr = RegVal emptyStrReg, type_ = Str})
                declareItems ident val items accu
emitStmtInstr (Incr ident) = 
    emitExpr (EVar ident)

emitStmt :: Stmt -> Eval Env
emitStmt Empty = return ()
emitStmt (BStmt block) = emitBlock block
emitStmt (Ass ident expr) = do
    instr 
    registry <- getNextRegistry
    
