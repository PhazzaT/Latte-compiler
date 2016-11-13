module AST where

newtype Program = Program [FnDef] deriving (Eq, Show)

data FnDef = FnDef Type Identifier [Arg] Stmt deriving (Eq, Show)

data Arg = Arg Type Identifier deriving (Eq, Show)

data Stmt = Assign Identifier Expr
          | Block BlockID [Stmt]
          | Decl Type [Item]
          | Empty
          | If Expr Stmt Stmt
          | Return Expr
          | SExpr Expr
          | VReturn
          | While Expr Stmt
          deriving (Eq, Show)

data Item = ItemDeclDefault Identifier
          | ItemDecl Identifier Expr
          deriving (Eq, Show)

data Expr = ELogic Expr LogicOp Expr
          | EAdd Expr AddOp Expr
          | ERel Expr RelOp Expr
          | EMul Expr MulOp Expr
          | Not Expr
          | Neg Expr
          | EString String
          | EApp Identifier [Expr]
          | EBoolLiteral Bool
          | EIntLiteral Integer
          | EVar Identifier
          deriving (Eq, Show)

data LogicOp = Or | And deriving (Eq, Show)
data AddOp = Plus | Minus deriving (Eq, Show)
data MulOp = Multiply | Divide | Modulo deriving (Eq, Show)
data RelOp = Less | LessEqual | Greater | GreaterEqual | Equal | NotEqual deriving (Eq, Show)

data Type = TNamed Identifier
          | TFunction Type [Type]
          deriving (Eq, Show)

type BlockID = Integer
type Identifier = String

globalScopeID :: BlockID
globalScopeID = 0

