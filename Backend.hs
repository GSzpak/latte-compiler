module Backend where


import AbsLatte
import qualified Data.Map as Map
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Text.Printf


type Name = String
type Registry = Name
type RegCounter = Integer

data ExpValRepr =
    RegVal Name |
    NumVal Integer |
    BoolVal Bool |
    StrVal String
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
    ExprOperation BinOp Type ExpVal ExpVal |
    Concat Registry Registry |
    Load Type Registry |
    Call Type Name [ExpVal]
    
    
data LLVmInstr = LLVmInstr {
    resultReg :: Registry
    instr :: Instruction
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
    instructions :: [LLVmInstr],
    regCounter :: RegCounter
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
        instructions = instructions store
        regCounter = actRegNum + 1
    }
    return $ getRegistry actRegNum

addInstruction :: Instruction -> Eval ()
addInstruction instr = do
    store <- get
    put $ Store {
        instructions = (instructions store) ++ instr,
        regCounter = regCounter store
    }



emitExpr :: Expr -> Eval ExpVal
emitExpr (EVar ident) = do
    env <- ask
    let Just (EnvElem reg t) = Map.lookup ident (varEnv env)
    result <- getNextRegistry
    let instr = Load t reg
    addInstruction $ LLVmInstr {resultReg = result, instr = instr}
    return $ ExpVal {repr = Reg result, type_ = t}
emitExpr (ELitInt n) = return $ ExpVal {repr = NumVal n, type_ = Int}
emitExpr ELitTrue = return $ ExpVal {repr = BoolVal True, type_ = Bool}
emitExpr ELitFalse = return $ ExpVal {repr = BoolVal False, type_ = Bool}
emitExpr (EApp ident args) = do
    env <- ask
    let Just (EnvElem name t) = Map.lookup ident (funEnv env)
    argReprs <- mapM emitExpr args
    result <- getNextRegistry
    let instr = Call t name argReprs
    addInstruction $ LLVmInstr {resultReg = result, instr = instr}
    return result
emitExpr (Neg expr) = emitExpr (EAdd (ELitInt 0) Minus expr)
emitExpr (Not expr) = do
    repr <- emitExpr expr
    result <- getNextRegistry
    let instr = Not Registry
    addInstruction
