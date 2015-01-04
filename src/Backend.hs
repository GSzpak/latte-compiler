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
    BoolVal Bool

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

data BinOp = Add | Sub | Mul | DivOp | ModOp

instance Show BinOp where
    show Add = "add"
    show Sub = "sub"
    show Mul = "mul"
    show DivOp = "sdiv"
    show ModOp = "srem"

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
    CondJump ExpVal LabelNum LabelNum |
    Jump LabelNum |
    Phi Registry Type [(ExpVal, LabelNum)] |
    GetElementPtr Registry Integer Name |
    FunDecl Name Type [Type]

instance Show Instruction where
    show (BinOpExpr result binop val1 val2) =
        let
            typeRepr = showLLVMType $ type_ val1
            val1Repr = show $ repr val1
            val2Repr = show $ repr val2
        in
            printf "%s = %s %s %s, %s" result (show binop) typeRepr val1Repr val2Repr
    show (RelOpExpr result relop val1 val2) =
        let
            typeRepr = showLLVMType $ type_ val1
            val1Repr = show $ repr val1
            val2Repr = show $ repr val2
        in
            printf "%s = icmp %s %s %s, %s" result (showLLVMRelOp relop) typeRepr val1Repr val2Repr
    show (NotExpr result val) =
        printf "%s = xor i1 %s, true" result (show $ repr val)
    show (Load result type_ reg) = 
        printf "%s = load %s* %s" result (showLLVMType type_) reg
    show (Call result type_ fun args) = 
        printf "%s = call %s %s(%s)" result (showLLVMType type_) fun (showFunArgs args)
    show (StoreInstr val reg) =
        printf "store %s, %s* %s" (show val) (showLLVMType $ type_ val) reg
    show (Alloca reg type_) =
        printf "%s = alloca %s" reg (show type_)
    show VoidRet = "ret void"
    show (ExpRet val) = printf "ret %s" (show val)
    show (CondJump val labelNum1 labelNum2) =
        printf "br %s, label %%s, label %s" (show val) ('%':(label labelNum1)) (label labelNum2)
    show (Jump labelNum) = printf "br label %%s" (label labelNum)
    show (Phi result type_ exprsFromLabels) = 
        printf "%s = phi %s %s" result (show type_) (showPhiExprs exprsFromLabels)
    show (GetElementPtr resultReg length constName) = 
        printf "%s = getelementptr inbounds [%s x i8]* %s, i32 0, i32 0" resultReg (show length) constName
    show (FunDecl name retType argTypes) =
        let
            showArgTypes = printWithSeparator (map showLLVMType argTypes) ","
        in 
            printf "declare %s @%s(%s)" (showLLVMType retType) name showArgTypes

data LLVMBlock = LLVMBlock {
    labelNum :: LabelNum,
    instructions :: [Instruction],
    lastInstr :: Maybe Instruction
}

data EnvElem = EnvElem Name Type

type Env = Map.Map Ident EnvElem

data Environment = Environment {
    varEnv :: Env,
    funEnv :: Env
}

data Store = Store {
    blocks :: Map.Map LabelNum LLVMBlock,
    actBlockNum :: LabelNum,
    regCounter :: Counter,
    constCounter :: Counter,
    labelCounter :: Counter,
    strConstants :: Map.Map String Name,
    compiled :: [String]
}

type Eval a = ReaderT Environment (ErrorT String (StateT Store IO)) a

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

runEval :: Environment -> Store -> Eval a -> IO (Either String a, Store)
runEval env state eval = runStateT (runErrorT (runReaderT eval env)) state

regName :: Counter -> Registry
regName regNum = "%r" ++ (show regNum)

globalName :: Ident -> Name
globalName (Ident name) = '@':name

constName :: Counter -> Name
constName strNum = printf "@.str%s" (show strNum)

label :: LabelNum -> Label
label num = printf "label%s" (show num)

showLLVMRelOp :: RelOp -> String
showLLVMRelOp LTH = "slt"
showLLVMRelOp LE = "sle"
showLLVMRelOp GTH = "sgt"
showLLVMRelOp GE = "sge"
showLLVMRelOp EQU = "eq"
showLLVMRelOp NE = "ne"

showLLVMType :: Type -> String
showLLVMType Int = "i32"
showLLVMType Str = "i8*"
showLLVMType Bool = "i1"
showLLVMType Void = "void"

printWithSeparator :: [String] -> String -> String
printWithSeparator strings sep = unwords $ List.intersperse "," strings

showFunArgs :: [ExpVal] -> String
showFunArgs args = printWithSeparator (map show args) ","

showPhiExprs :: [(ExpVal, LabelNum)] -> String
showPhiExprs exprsWithLabels = printWithSeparator (map show' exprsWithLabels) ","
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

getBlock :: LabelNum -> Eval LLVMBlock
getBlock labelNum = do
    store <- get
    let Just block = Map.lookup labelNum (blocks store)
    return block
    
getActBlock :: Eval LLVMBlock
getActBlock = do
    store <- get
    let Just actBlock = Map.lookup (actBlockNum store) (blocks store)
    return actBlock

getActBlockNum :: Eval LabelNum
getActBlockNum = do
    store <- get
    return $ actBlockNum store

changeBlock :: LabelNum -> (LLVMBlock -> LLVMBlock) -> Eval ()
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
    setLastInstructionInBlock lastInstr (actBlockNum store)

addNewLLVMBlock :: Eval LabelNum
addNewLLVMBlock = do
    store <- get
    let next = (actBlockNum store) + 1
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
        labelCounter = labelCounter store,
        strConstants = strConstants store,
        compiled = compiled store
    }
    return next

emitBinOpInstruction :: Expr -> Expr -> BinOp -> Registry -> Eval (Type, Instruction)
emitBinOpInstruction e1 e2 operator resultReg = do
    val1 <- emitExpr e1
    val2 <- emitExpr e2
    return (Int, BinOpExpr resultReg operator val1 val2)

emitRelOpInstruction :: Expr -> Expr -> RelOp -> Registry -> Eval (Type, Instruction)
emitRelOpInstruction e1 e2 relOp resultReg = do
    val1 <- emitExpr e1
    val2 <- emitExpr e2
    return (Bool, RelOpExpr resultReg relOp val1 val2)

-- TODO: useless
{-
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
-}

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
    emitExprInstruction (EAdd Minus (ELitInt 0) expr) resultReg
emitExprInstruction (Not expr) resultReg = do
    val <- emitExpr expr
    return (Bool, NotExpr resultReg val)
emitExprInstruction (EMul Times expr1 expr2) resultReg =
    emitBinOpInstruction expr1 expr2 Mul resultReg
emitExprInstruction (EMul Div expr1 expr2) resultReg =
    emitBinOpInstruction expr1 expr2 DivOp resultReg
emitExprInstruction (EMul Mod expr1 expr2) resultReg =
    emitBinOpInstruction expr1 expr2 ModOp resultReg
-- Adding is handled separately in emitExpr
emitExprInstruction (EAdd Minus expr1 expr2) resultReg =
    emitBinOpInstruction expr1 expr2 Sub resultReg
emitExprInstruction (ERel LTH expr1 expr2) resultReg =
    emitRelOpInstruction expr1 expr2 LTH resultReg
emitExprInstruction (ERel LE expr1 expr2) resultReg =
    emitRelOpInstruction expr1 expr2 LE resultReg
emitExprInstruction (ERel GTH expr1 expr2) resultReg =
    emitRelOpInstruction expr1 expr2 GTH resultReg
emitExprInstruction (ERel GE expr1 expr2) resultReg =
    emitRelOpInstruction expr1 expr2 GE resultReg
emitExprInstruction (ERel EQU expr1 expr2) resultReg =
    emitRelOpInstruction expr1 expr2 EQU resultReg
emitExprInstruction (ERel NE expr1 expr2) resultReg =
    emitRelOpInstruction expr1 expr2 NE resultReg

emitExprToBlock :: Expr -> LabelNum -> Eval ExpVal
emitExprToBlock (EAnd expr1 expr2) labelNum = do
    val1 <- emitExprToBlock expr1 labelNum
    numTrue <- addNewLLVMBlock
    val2 <- emitExprToBlock expr2 numTrue
    numNext <- addNewLLVMBlock
    setLastInstructionInBlock (CondJump val1 numTrue numNext) labelNum
    setLastInstructionInBlock (Jump numNext) numTrue
    resultReg <- getNextRegistry
    addInstructionToBlock (Phi resultReg Bool [(falseExpVal, labelNum), (val2, numTrue)]) numNext
    return $ ExpVal {repr = RegVal resultReg, type_ = Bool}
emitExprToBlock (EOr expr1 expr2) labelNum = do
    val1 <- emitExprToBlock expr1 labelNum
    numFalse <- addNewLLVMBlock
    val2 <- emitExprToBlock expr2 numFalse
    numNext <- addNewLLVMBlock
    setLastInstructionInBlock (CondJump val1 numNext numFalse) labelNum
    setLastInstructionInBlock (Jump numNext) numFalse
    resultReg <- getNextRegistry
    addInstructionToBlock (Phi resultReg Bool [(trueExpVal, labelNum), (val2, numFalse)]) numNext
    return $ ExpVal {repr = RegVal resultReg, type_ = Bool}
emitExprToBlock expr labelNum = do
    result <- getNextRegistry
    (t, instr) <- emitExprInstruction expr result
    addInstructionToBlock instr labelNum
    return $ ExpVal {repr = RegVal result, type_ = t}

llvmStrLen :: String -> Integer
llvmStrLen s = (toInteger $ length s) + 1

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

emitConcat :: ExpVal -> ExpVal -> Eval ExpVal
emitConcat val1 val2 = do
    (lenReg1, lenVal1) <- getExpVal Int
    addInstruction $ Call lenReg1 Int (globalName' "strlen") [lenVal1]
    (lenReg2, lenVal2) <- getExpVal Int
    addInstruction $ Call lenReg2 Int (globalName' "strlen") [lenVal2]
    (tempReg1, tempVal1) <- getExpVal Int
    addInstruction $ BinOpExpr tempReg1 Add lenVal1 (numExpVal 1)
    (tempReg2, tempVal2) <- getExpVal Int
    addInstruction $ BinOpExpr tempReg2 Add tempVal1 lenVal2
    (mallocReg, mallocVal) <- getExpVal Str
    addInstruction $ Call mallocReg Str (globalName' "malloc") [tempVal2]
    (strcpyReg, strcpyVal) <- getExpVal Str
    addInstruction $ Call strcpyReg Str (globalName' "strcpy") [mallocVal, val1]
    (strcatReg, strcatVal) <- getExpVal Str
    addInstruction $ Call strcatReg Str (globalName' "strcat") [strcpyVal, val2]
    return strcatVal
    where
        getExpVal :: Type -> Eval (Registry, ExpVal)
        getExpVal t = do
            reg <- getNextRegistry
            let val = ExpVal {repr = RegVal reg, type_ = t}
            return $ (reg, val)
        globalName' :: String -> Name
        globalName' s = globalName (Ident s)

emitExpr :: Expr -> Eval ExpVal
emitExpr (ELitInt n) = return $ numExpVal n
emitExpr ELitTrue = return $ boolExpVal True
emitExpr ELitFalse = return $ boolExpVal False
emitExpr (EString s) = do
    store <- get
    name <- getStringName s
    registry <- getNextRegistry
    addInstruction $ GetElementPtr registry (llvmStrLen s) name
    return $ ExpVal {repr = RegVal registry, type_ = Str}
emitExpr (EAdd Plus expr1 expr2) = do
    val1 <- emitExpr expr1 
    val2 <- emitExpr expr2
    -- after type checking
    case type_ val1 of
        Str -> emitConcat val1 val2
        Int -> do
            resultReg <- getNextRegistry
            addInstruction $ BinOpExpr resultReg Add val1 val2
            return $ ExpVal {repr = RegVal resultReg, type_ = Int}
emitExpr expr = do
    actBlock <- getActBlockNum
    emitExprToBlock expr actBlock

emitDeclarations :: Type -> [Item] -> [Instruction] -> Eval (Environment, [Instruction])
emitDeclarations type_ [] accu = do
    env <- ask
    return (env, accu)
emitDeclarations t (item:items) accu = case item of
    Init ident expr -> (do
        val <- emitExpr expr
        (env, updatedAccu) <- declareItem ident val accu
        local (\_ -> env) (emitDeclarations t items updatedAccu))
    NoInit ident -> do
        emptyStrReg <- getStringName ""
        val <- case t of
            Int -> return $ numExpVal 0
            Bool -> return falseExpVal
            Str -> return $ ExpVal {repr = RegVal emptyStrReg, type_ = Str}
        (env, updatedAccu) <- declareItem ident val accu
        local (\_ -> env) (emitDeclarations t items updatedAccu)
    where
        declareItem :: Ident -> ExpVal -> [Instruction] -> Eval (Environment, [Instruction])
        declareItem ident val accu = do
            env <- ask
            reg <- getNextRegistry
            let env' = Environment {
                varEnv = Map.insert ident (EnvElem reg t) (varEnv env),
                funEnv = funEnv env
            }
            let alloca = Alloca reg t
            let store = StoreInstr val reg
            return (env, (store:(alloca:accu)))

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
    val <- emitExprToBlock expr actBlock
    setLastInstructionInBlock (CondJump val trueBlock falseBlock) actBlock

emitStmt :: Stmt -> Eval Environment
emitStmt Empty = ask
emitStmt (BStmt block) = emitBlock block
emitStmt (Ass ident expr) = do
    env <- ask
    val <- emitExpr expr
    let Just (EnvElem reg t) = Map.lookup ident (varEnv env)
    addInstruction $ StoreInstr val reg
    ask
emitStmt (Decl type_ items) = do
    (env, instructions) <- emitDeclarations type_ items []
    addInstructions instructions
    return env
emitStmt (Incr ident) =
    emitStmt $ Ass ident (EAdd Plus (EVar ident) (ELitInt 1))
emitStmt (Decr ident) =
    emitStmt $ Ass ident (EAdd Minus (EVar ident) (ELitInt 1))
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
        setLastIfNecessary :: LabelNum -> Instruction -> Eval ()
        setLastIfNecessary blockNum lastInstruction = do
            block <- getBlock blockNum
            case lastInstr block of
                Just _ -> return ()
                Nothing -> setLastInstructionInBlock lastInstruction blockNum
emitStmt (CondElse expr stmt1 stmt2) = do
    actBlockNum <- getActBlockNum
    trueBlockNum <- addNewLLVMBlock
    emitStmt stmt1
    falseBlockNum <- addNewLLVMBlock
    emitStmt stmt2
    emitCondExpr expr actBlockNum trueBlockNum falseBlockNum
    newBlock <- jumpToNewBlock trueBlockNum
    case newBlock of
        Nothing -> ask
        Just afterNum -> do
            setLastInstructionInBlock (Jump afterNum) falseBlockNum
            ask
    where
        jumpToNewBlock :: LabelNum -> Eval (Maybe LabelNum)
        jumpToNewBlock blockNum = do
            block <- getBlock blockNum
            case lastInstr block of
                Just _ -> return Nothing
                Nothing -> do
                    newBlock <- addNewLLVMBlock
                    setLastInstructionInBlock (Jump newBlock) blockNum
                    return $ Just newBlock
emitStmt (While expr stmt) = do
    actBlockNum <- getActBlockNum
    loopBodyNum <- addNewLLVMBlock
    emitStmt stmt
    -- TODO: while(true) { return 1;}
    loopCondNum <- addNewLLVMBlock
    setLastInstructionInBlock (Jump loopCondNum) actBlockNum
    afterLoop <- addNewLLVMBlock
    emitCondExpr expr actBlockNum loopBodyNum afterLoop
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

identReg :: Ident -> Registry
identReg (Ident name) = '%':name

showLLVMArgs :: [Arg] -> String
showLLVMArgs args = printWithSeparator (map showLLVMArg args) ","
    where
        showLLVMArg :: Arg -> String
        showLLVMArg (Arg type_ ident) = printf "%s %s" (showLLVMType type_) (identReg ident)

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

compileBlock :: LLVMBlock -> [String]
compileBlock block = ["", formatInstr $ lastInstruction] ++ (map formatInstr (instructions block))
    where
        Just lastInstruction = lastInstr block
        formatInstr :: Instruction -> String
        formatInstr instr = printf "      %s" (show instr)

compileBlocksInstructions :: Eval ()
compileBlocksInstructions = do
    store <- get
    sequence_ $ map compileBlockWithLabel (Map.toDescList $ blocks store)
    put $ Store {
        blocks = Map.empty,
        actBlockNum = actBlockNum store,
        regCounter = regCounter store,
        constCounter = constCounter store,
        labelCounter = labelCounter store,
        strConstants = strConstants store,
        compiled = compiled store
    }
    where
        compileBlockWithLabel :: (LabelNum, LLVMBlock) -> Eval ()
        compileBlockWithLabel (labelNum, block) = do
            addCompiled [formatLabel labelNum]
            addCompiled $ compileBlock block
        formatLabel :: LabelNum -> String
        formatLabel labelNum = printf "   %s" (label labelNum)

emitTopDef :: TopDef -> Eval ()
emitTopDef (FnDef type_ ident args block) = do
    addCompiled ["", "}"]
    env <- addArgs args
    addNewLLVMBlock
    local (\_ -> env) (emitBlock block)
    compileBlocksInstructions
    let funHeader = printf "define %s %s(%s) {" (showLLVMType type_) (globalName ident) (showLLVMArgs args)
    addCompiled [funHeader]

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
    
addOuterDefinitions :: Eval ()
addOuterDefinitions = addCompiled $ map show declarations
    where
        declarations =
            [FunDecl "printInt" Void [Int],
            FunDecl "printString" Void [Str],
            FunDecl "error" Void [],
            FunDecl "readInt" Int [],
            FunDecl "readString" Str [],
            FunDecl "malloc" Str [Int],
            FunDecl "strlen" Int [Str],
            FunDecl "strcpy" Str [Str, Str],
            FunDecl "strcat" Str [Str, Str]]

addStringsDefinitions :: Eval ()
addStringsDefinitions = do
    store <- get
    addCompiled $ map getStringDecl (Map.toList (strConstants store))
    where
        getStringDecl :: (String, Name) -> String
        getStringDecl (s, name) =
            printf "%s = private constant [%s x i8] c\"%s\00\"" name (llvmStrLen s) (concat $ map llvmFormat s)
        llvmFormat :: Char -> String
        llvmFormat '\t' = "\09"
        llvmFormat '\n' = "\0a"
        llvmFormat c = [c]

emitProgram :: Program -> Eval ()
emitProgram (Program topDefs) = do
    getStringName ""
    sequence_ $ map emitTopDef (reverse topDefs)
    reverseInstructions
    addOuterDefinitions
    addStringsDefinitions
