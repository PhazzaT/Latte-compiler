{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module AST where

import Data.List
import Data.String
import Data.Generics

newtype ProgramF i = Program [FnDefF i] deriving (Eq, Data, Typeable, Show)
type Program = ProgramF Identifier
type ProgramTyped = ProgramF MangledIdentifier

data FnDefF i = FnDef Type i [ArgF i] (StmtF i) deriving (Eq, Data, Typeable, Show)
type FnDef = FnDefF Identifier
type FnDefTyped = FnDefF MangledIdentifier

data ArgF i = Arg Type i deriving (Eq, Data, Typeable, Show)
type Arg = ArgF Identifier
type ArgTyped = ArgF MangledIdentifier

data StmtF i = Assign (ExprF i) (ExprF i)
             | Block BlockID [StmtF i]
             | Decl Type [ItemF i]
             | Empty
             | If (ExprF i) (StmtF i) (StmtF i)
             | Return (ExprF i)
             | SExpr (ExprF i)
             | VReturn
             | While (ExprF i) (StmtF i)
             deriving (Eq, Data, Typeable, Show)
type Stmt = StmtF Identifier
type StmtTyped = StmtF MangledIdentifier

data ItemF i = Item i (Maybe (ExprF i)) deriving (Eq, Data, Typeable, Show)
type Item = ItemF Identifier
type ItemTyped = ItemF MangledIdentifier

data ExprF i = EString String
             | EApp i [ExprF i]
             | EBoolLiteral Bool
             | EIntLiteral Integer
             | ENew Type [ExprF i]
             | EVar i
             deriving (Eq, Data, Typeable, Show)
type Expr = ExprF Identifier
type ExprTyped = ExprF MangledIdentifier

data LogicOp = Or | And deriving (Eq, Data, Typeable)
data AddOp = Plus | Minus deriving (Eq, Data, Typeable)
data MulOp = Multiply | Divide | Modulo deriving (Eq, Data, Typeable)
data RelOp = Less | LessEqual | Greater | GreaterEqual | Equal | NotEqual deriving (Eq, Data, Typeable)

instance Show LogicOp where
    show Or = "||"
    show And = "&&"

instance Show AddOp where
    show Plus = "+"
    show Minus = "-"

instance Show MulOp where
    show Multiply = "*"
    show Divide = "/"
    show Modulo = "%"

instance Show RelOp where
    show Less = "<"
    show LessEqual = "<="
    show Greater = ">"
    show GreaterEqual = ">="
    show Equal = "=="
    show NotEqual = "!="

data Type = TNamed String
          | TArray Type
          | TFunction Type [Type]
          deriving (Eq, Ord, Data, Typeable)

instance Show Type where
    show = typeMangle

type BlockID = Integer
-- newtype Identifier = ID { unID :: String } deriving (Eq, Data, Typeable, Ord, Show)
type Identifier = String

data MangledIdentifier = MangledIdentifier { identifierLabel :: Identifier
                                           , identifierType  :: Type
                                           , identifierScope :: [BlockID]
                                           } deriving (Eq, Data, Ord, Show, Typeable)


globalScopeID :: BlockID
globalScopeID = 0


-- mangle :: Identifier -> BlockID -> Type -> Identifier
-- mangle i bid t = i ++ "/" ++ show bid ++ "/" ++ typeMangle t


typeMangle :: Type -> String
typeMangle (TNamed n) = n
typeMangle (TArray t) = '@' : typeMangle t
typeMangle (TFunction tRet ts) = "\\" ++ typeMangle tRet ++ "(" ++ intercalate "," (map typeMangle ts) ++ ")"


tVoid :: Type
tVoid = TNamed "void"

tBool :: Type
tBool = TNamed "boolean"

tString :: Type
tString = TNamed "string"

tInt :: Type
tInt = TNamed "int"

