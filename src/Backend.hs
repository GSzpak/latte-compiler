module Backend where


import AbsLatte
import qualified Data.List as List
import qualified Data.Map as Map
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
    CondJump ExpVal BlockNum BlockNum |
    Jump BlockNum |
    Phi Registry Type [(ExpVal, BlockNum)] |
    GetElementPtr Registry Integer Name |
    FunDecl Name Type [Type]

identReg :: Ident -> Registry
identReg (Ident name) = '%':name

regName :: Counter -> Registry
regName regNum = "%r" ++ (show regNum)

globalName :: Ident -> Name
globalName (Ident name) = '@':name

constName :: Counter -> Name
constName strNum = printf "@.str%s" (show strNum)

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

printWithSeparator :: [String] -> String -> String
printWithSeparator strings sep = unwords $ List.intersperse "," strings

showFunArgs :: [ExpVal] -> String
showFunArgs args = printWithSeparator (map show args) ","

showPhiExprs :: [(ExpVal, BlockNum)] -> String
showPhiExprs exprsWithLabels = printWithSeparator (map show' exprsWithLabels) ","
    where
        show' :: (ExpVal, BlockNum) -> String
        show' (val, num) = printf "[ %s, %s ]" (show $ repr val) ('%':(label num))

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
    show (GetElementPtr resultReg length constName) = 
        printf "%s = getelementptr inbounds [%s x i8]* %s, i32 0, i32 0" resultReg (show length) constName
    show (FunDecl name retType argTypes) =
        let
            showArgTypes = printWithSeparator (map showLLVMType argTypes) ","
        in 
            printf "declare %s @%s(%s)" (showLLVMType retType) name showArgTypes

data LLVMBlock = LLVMBlock {
    labelNum :: BlockNum,
    instructions :: [Instruction],
    lastInstr :: Maybe Instruction
} deriving Show

data EnvElem = EnvElem Name Type deriving Show

type Env = Map.Map Ident EnvElem

data Environment = Environment {
    varEnv :: Env,
    funEnv :: Env
} deriving Show

data Store = Store {
    blocks :: Map.Map BlockNum LLVMBlock,
    actBlockNum :: BlockNum,
    regCounter :: Counter,
    constCounter :: Counter,
    labelCounter :: Counter,
    strConstants :: Map.Map String Name,
    compiled :: [String]
} deriving Show

type Eval a = ReaderT Environment (StateT Store IO) a

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
emitExprInstruction (EVar ident) resultReg = do
    env <- ask
    let Just (EnvElem reg t) = Map.lookup ident (varEnv env)
    addInstruction $ Load resultReg t reg
    return t
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

emitExpr :: Expr -> Eval ExpVal
emitExpr (ELitInt n) = return $ numExpVal n
emitExpr ELitTrue = return $ boolExpVal True
emitExpr ELitFalse = return $ boolExpVal False
emitExpr (EString s) = do
    name <- getStringName s
    registry <- getNextRegistry
    addInstruction $ GetElementPtr registry (llvmStrLen s) name
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
emitExpr expr = do
    result <- getNextRegistry
    t <- emitExprInstruction expr result
    return $ ExpVal {repr = RegVal result, type_ = t}

declare :: Ident -> ExpVal -> Eval Environment
declare ident val = do
    env <- ask
    reg <- getNextRegistry
    let t = type_ val
    let env' = Environment {
        varEnv = Map.insert ident (EnvElem reg t) (varEnv env),
        funEnv = funEnv env
    }
    let alloca = Alloca reg t
    let store = StoreInstr val reg
    addInstructions [store, alloca]
    return env'

emitDeclarations :: Type -> [Item] -> Eval Environment
emitDeclarations _ [] = ask
emitDeclarations t (item:items) = case item of
    Init ident expr -> (do
        env <- ask
        val <- emitExpr expr
        env <- declare ident val
        local (\_ -> env) (emitDeclarations t items))
    NoInit ident -> do
        val <- case t of
            Int -> return $ numExpVal 0
            Bool -> return falseExpVal
            Str -> emitExpr $ EString ""
        env <- declare ident val
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
    let Just (EnvElem reg t) = Map.lookup ident (varEnv env)
    addInstruction $ StoreInstr val reg
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

emitTopDef :: TopDef -> Eval ()
emitTopDef (FnTopDef (FnDef t ident args block)) = do
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
    env' <- local (\_ -> env) (declareFunctions topDefs)
    local (\_ -> env') (sequence_ $ map emitTopDef topDefs)
    reverseInstructions
    addCompiled [""]
    addOuterDeclarations
    addCompiled [""]
    addStringsDefinitions
