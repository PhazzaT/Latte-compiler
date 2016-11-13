module CompilePasses ( compile
                     , lexAndParse
                     , typeCheck
                     , SymbolLocation(SLoc)
                     ) where

import Control.Monad((>=>))

import AST
import Lexer
import Parser
import TypeCheck


compile :: String -> Either String String
compile = lexAndParse >=> typeCheck >=> return . show


lexAndParse :: String -> Either String Program
lexAndParse = tokenize >=> parse


typeCheck :: Program -> Either String (Program, TypeInfo)
typeCheck = buildTypeInformation

