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
data Env = Env {varEnv :: VarEnv, funEnv :: FunEnv, actBlockDepth :: BlockDepth}
type TypeEval a = ReaderT Env (ErrorT String IO) a


runTypeEval :: Env -> TypeEval a -> IO (Either String IO) a
runTypeEval env eval = runErrorT (runReaderT eval env)

name :: Ident -> String
name (Ident s) = s

incomaptibleTypesErr :: Type -> Type -> String
incomaptibleTypesErr expected actual =
    "Incompatible types: expected " ++ (printTree expected) ++ ", got " ++ (printTree actual)

evalExprType :: Expr -> TypeEval Type
evalExprType (EVar ident) = do
    env <- ask
    case Map.lookup ident (varEnv) of
        Just t -> return t
        Nothing -> "Undeclared variable: " ++ (name ident)
evalExprType (ElitInt _) = return Int
evalExprType ElitTrue = return Bool
evalExprType ELitFalse = return Bool
evalExprType (EString _) = return Str
evalExprType (Neg expr) = do
    t <- evalExprType expr
    if t /= Int then
        throwError $ incomaptibleTypesErr Int t
    else
        --- t == Int
        return t
evalExprType (Not expr) = do
    t <- evalExprType expr
    if t /= Bool then 
        throwError $ incomaptibleTypesErr Bool t

        
