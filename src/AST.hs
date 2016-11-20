{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module AST where

import Data.List
import Data.String
import Data.Generics

newtype Program = Program [FnDef] deriving (Eq, Data, Typeable, Show)

data FnDef = FnDef Type Identifier [Arg] Stmt deriving (Eq, Data, Typeable, Show)

data Arg = Arg Type Identifier deriving (Eq, Data, Typeable, Show)

data Stmt = Assign Identifier Expr
          | Block BlockID [Stmt]
          | Decl Type [Item]
          | Empty
          | If Expr Stmt Stmt
          | Return Expr
          | SExpr Expr
          | VReturn
          | While Expr Stmt
          deriving (Eq, Data, Typeable, Show)

data Item = Item Identifier (Maybe Expr) deriving (Eq, Data, Typeable, Show)

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
          deriving (Eq, Data, Typeable, Show)

data LogicOp = Or | And deriving (Eq, Data, Typeable, Show)
data AddOp = Plus | Minus deriving (Eq, Data, Typeable, Show)
data MulOp = Multiply | Divide | Modulo deriving (Eq, Data, Typeable, Show)
data RelOp = Less | LessEqual | Greater | GreaterEqual | Equal | NotEqual deriving (Eq, Data, Typeable, Show)

data Type = TNamed String
          | TFunction Type [Type]
          deriving (Eq, Data, Typeable, Show)

type BlockID = Integer
newtype Identifier = ID String deriving (Eq, Data, Typeable, Ord, Show)

instance IsString Identifier where
    fromString = ID


globalScopeID :: BlockID
globalScopeID = 0


mangle :: Identifier -> BlockID -> Type -> Identifier
mangle (ID i) bid t = ID $ i ++ "/" ++ show bid ++ "/" ++ typeMangle t


typeMangle :: Type -> String
typeMangle (TNamed n) = '#' : n
typeMangle (TFunction tRet ts) = "\\" ++ typeMangle tRet ++ "(" ++ intercalate "," (map typeMangle ts) ++ ")"

