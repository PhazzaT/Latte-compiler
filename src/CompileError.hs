{-# LANGUAGE DeriveDataTypeable #-}
module CompileError where

import Data.Generics


data CompileError = CompileError CompileErrorType String deriving (Eq, Ord, Data)

data CompileErrorType
    = ParseError 
    | TypeCheckError 
    | CodeGenerationError
    deriving (Eq, Ord, Data)


instance Show CompileError where
    show (CompileError t msg) = show t ++ ": " ++ msg


instance Show CompileErrorType where
    show ParseError = "Parse error"
    show TypeCheckError = "Type error"
    show CodeGenerationError = "Code generation error"


parseError :: String -> CompileError
parseError = CompileError ParseError


typeCheckError :: String -> CompileError
typeCheckError = CompileError TypeCheckError


codeGenerationError :: String -> CompileError
codeGenerationError = CompileError CodeGenerationError

