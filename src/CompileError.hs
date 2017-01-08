{-# LANGUAGE DeriveDataTypeable #-}
module CompileError where

import Data.Generics

import AST

data CompileError = CompileError CompileErrorType PhaseError deriving (Eq, Ord, Data)
data PhaseError = PhaseError LocInfo String deriving (Eq, Ord, Data)

data CompileErrorType
    = ParseError 
    | TypeCheckError 
    | StaticAnalysisError
    | CodeGenerationError
    deriving (Eq, Ord, Data)


instance Show PhaseError where
    show (PhaseError NoLocInfo s)       = s
    show (PhaseError (LocInfo p1 p2) s) = show p1 ++ " - " ++ show p2 ++ " " ++ s


instance Show CompileError where
    show (CompileError t msg) = show t ++ ": " ++ show msg


instance Show CompileErrorType where
    show ParseError = "Parse error"
    show TypeCheckError = "Type error"
    show StaticAnalysisError = "Static analysis error"
    show CodeGenerationError = "Code generation error"


parseError :: PhaseError -> CompileError
parseError = CompileError ParseError


typeCheckError :: PhaseError -> CompileError
typeCheckError = CompileError TypeCheckError


staticAnalysisError :: PhaseError -> CompileError
staticAnalysisError = CompileError StaticAnalysisError


codeGenerationError :: PhaseError -> CompileError
codeGenerationError = CompileError CodeGenerationError

