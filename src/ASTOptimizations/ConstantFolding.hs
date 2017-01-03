module ASTOptimizations.ConstantFolding(constantFolding) where

import Data.Generics

import AST


constantFolding :: ProgramTyped -> ProgramTyped
constantFolding = everywhere $ mkT cfExpr
    where
        cfExpr :: ExprTyped -> ExprTyped
        cfExpr (EApp ident [EBoolLiteral b1, EBoolLiteral b2])
            | identifierLabel ident == "||" = EBoolLiteral $ b1 || b2
            | identifierLabel ident == "&&" = EBoolLiteral $ b1 && b2
            | identifierLabel ident == "==" = EBoolLiteral $ b1 == b2
            | identifierLabel ident == "/=" = EBoolLiteral $ b1 /= b2
        cfExpr (EApp ident [EBoolLiteral False, e2])
            | identifierLabel ident == "||" = e2
        cfExpr (EApp ident [EBoolLiteral True, e2])
            | identifierLabel ident == "&&" = e2
        cfExpr e@(EApp ident [EIntLiteral i1, EIntLiteral i2])
            | identifierLabel ident == "+" = EIntLiteral $ i1 + i2
            | identifierLabel ident == "-" = EIntLiteral $ i1 - i2
            | identifierLabel ident == "*" = EIntLiteral $ i1 * i2
            | identifierLabel ident == "/" =
                if i2 == 0
                    then e
                    else EIntLiteral $ i1 `div` i2
            | identifierLabel ident == "%" =
                if i2 == 0
                    then e
                    else EIntLiteral $ i1 `mod` i2
            | identifierLabel ident == "<"  = EBoolLiteral $ i1 <  i2
            | identifierLabel ident == "<=" = EBoolLiteral $ i1 <= i2
            | identifierLabel ident == ">"  = EBoolLiteral $ i1 >  i2
            | identifierLabel ident == ">=" = EBoolLiteral $ i1 >= i2
            | identifierLabel ident == "==" = EBoolLiteral $ i1 == i2
            | identifierLabel ident == "/=" = EBoolLiteral $ i1 /= i2
        cfExpr (EApp ident [EString s1, EString s2])
            | identifierLabel ident == "+" = EString $ s1 ++ s2
        cfExpr (EApp ident [EIntLiteral i])
            | identifierLabel ident == "-" = EIntLiteral (-i)
        cfExpr e = e

