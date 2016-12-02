{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module AST where

import Data.List
import Data.String
import Data.Generics

newtype ProgramF i = Program [FnDefF i] deriving (Eq, Data, Typeable, Show)
type Program = ProgramF Identifier

data FnDefF i = FnDef Type i [ArgF i] (StmtF i) deriving (Eq, Data, Typeable, Show)
type FnDef = FnDefF Identifier

data ArgF i = Arg Type i deriving (Eq, Data, Typeable, Show)
type Arg = ArgF Identifier

data StmtF i = Assign i (ExprF i)
             | Block BlockID [StmtF i]
             | Decl Type [ItemF i]
             | Empty
             | If Expr (StmtF i) (StmtF i)
             | Return (ExprF i)
             | SExpr (ExprF i)
             | VReturn
             | While (ExprF i) (StmtF i)
             deriving (Eq, Data, Typeable, Show)
type Stmt = StmtF Identifier

data ItemF i = Item i (Maybe (ExprF i)) deriving (Eq, Data, Typeable, Show)
type Item = ItemF Identifier

data ExprF i = ELogic (ExprF i) LogicOp (ExprF i)
             | EAdd (ExprF i) AddOp (ExprF i)
             | ERel (ExprF i) RelOp (ExprF i)
             | EMul (ExprF i) MulOp (ExprF i)
             | Not (ExprF i)
             | Neg (ExprF i)
             | EString String
             | EApp Identifier [ExprF i]
             | EBoolLiteral Bool
             | EIntLiteral Integer
             | EVar Identifier
             deriving (Eq, Data, Typeable, Show)
type Expr = ExprF Identifier

data LogicOp = Or | And deriving (Eq, Data, Typeable, Show)
data AddOp = Plus | Minus deriving (Eq, Data, Typeable, Show)
data MulOp = Multiply | Divide | Modulo deriving (Eq, Data, Typeable, Show)
data RelOp = Less | LessEqual | Greater | GreaterEqual | Equal | NotEqual deriving (Eq, Data, Typeable, Show)

data Type = TNamed String
          | TFunction Type [Type]
          deriving (Eq, Data, Typeable, Show)

type BlockID = Integer
newtype Identifier = ID { unID :: String } deriving (Eq, Data, Typeable, Ord, Show)

instance IsString Identifier where
    fromString = ID

data MangledIdentifier = MangledIdentifier { identifierLabel :: Identifier
                                           , identifierType  :: Type
                                           , identifierScope :: BlockID
                                           }


globalScopeID :: BlockID
globalScopeID = 0


mangle :: Identifier -> BlockID -> Type -> Identifier
mangle (ID i) bid t = ID $ i ++ "/" ++ show bid ++ "/" ++ typeMangle t


typeMangle :: Type -> String
typeMangle (TNamed n) = '@' : n
typeMangle (TFunction tRet ts) = "\\" ++ typeMangle tRet ++ "(" ++ intercalate "," (map typeMangle ts) ++ ")"


tVoid :: Type
tVoid = TNamed "void"

tBool :: Type
tBool = TNamed "bool"

tString :: Type
tString = TNamed "string"

tInt :: Type
tInt = TNamed "int"

