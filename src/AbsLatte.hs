module AbsLatte where

-- Haskell module generated by the BNF converter


newtype Ident = Ident String deriving (Eq,Ord,Show)
data Program =
   Program [TopDef]
  deriving (Eq,Ord,Show)

data TopDef =
   FnTopDef FnDef
 | ClsDef :qIdent [Field] [FnDef]
 | ClsExtDef Ident Ident [Field] [FnDef]
  deriving (Eq,Ord,Show)

data FnDef =
   FnDef Type Ident [Arg] Block
  deriving (Eq,Ord,Show)

data Arg =
   Arg Type Ident
  deriving (Eq,Ord,Show)

data Field = Field {
    getType :: Type,
    getIdent :: Ident
} deriving (Eq,Ord,Show)

data Block =
   Block [Stmt]
  deriving (Eq,Ord,Show)

data Stmt =
   Empty
 | BStmt Block
 | Decl Type [Item]
 | Ass Ident Expr
 | Incr Ident
 | Decr Ident
 | Ret Expr
 | VRet
 | Cond Expr Stmt
 | CondElse Expr Stmt Stmt
 | While Expr Stmt
 | SExp Expr
  deriving (Eq,Ord,Show)

data Item =
   NoInit Ident
 | Init Ident Expr
  deriving (Eq,Ord,Show)

data Type =
   Int
 | Str
 | Bool
 | Void
 | Char
 | Fun Type [Type]
 | Cls Ident
 | Ptr Type
 | Arr Int Type
  deriving (Eq,Ord,Show)

data Expr =
   ELitInt Integer
 | ELitTrue
 | ELitFalse
 | EApp Ident [Expr]
 | EString String
 | ENew Ident
 | ENull Ident
 | EMApp Ident Ident [Expr]
 | EVar Ident
 | Neg Expr
 | Not Expr
 | EMul Expr MulOp Expr
 | EAdd Expr AddOp Expr
 | ERel Expr RelOp Expr
 | EAnd Expr Expr
 | EOr Expr Expr
  deriving (Eq,Show)

instance Ord Expr where
    (ELitInt n1) <= (ELitInt n2) = (n1 <= n2)
    ELitTrue <= ELitFalse = False
    ELitFalse <= ELitTrue = True
    (EString s1) <= (EString s2) = (s1 <= s2)
    _ <= _ = False

data AddOp =
   Plus
 | Minus
  deriving (Eq,Ord,Show)

data MulOp =
   Times
 | Div
 | Mod
  deriving (Eq,Ord,Show)

data RelOp =
   LTH
 | LE
 | GTH
 | GE
 | EQU
 | NE
  deriving (Eq,Ord,Show)

