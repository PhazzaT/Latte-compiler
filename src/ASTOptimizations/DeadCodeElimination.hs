module ASTOptimizations.DeadCodeElimination(deadCodeElimination) where

import Data.Generics

import AST


deadCodeElimination :: ProgramTyped -> ProgramTyped
deadCodeElimination = everywhere' $ mkT dceStmt
    where
        dceStmt :: StmtTyped -> StmtTyped
        dceStmt (If (EBoolLiteral True)  s _) = s
        dceStmt (If (EBoolLiteral False) _ s) = s
        dceStmt (SExpr (EBoolLiteral _)) = Empty
        dceStmt (SExpr (EIntLiteral _)) = Empty
        dceStmt (SExpr (EString _ )) = Empty
        dceStmt (While (EBoolLiteral False) _) = Empty
        dceStmt e = e

