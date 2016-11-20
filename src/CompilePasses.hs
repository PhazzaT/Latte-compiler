module CompilePasses ( compile
                     , lexAndParse
                     , typeCheck
                     , SymbolLocation(SLoc)
                     , renameUnique
                     ) where

import Control.Monad((>=>))

import AST
import Lexer
import Parser
import TypeCheck
import RenameUnique


compile :: String -> Either String String
compile = lexAndParse >=> typeCheck >=> uncurry renameUnique >=> return . show


lexAndParse :: String -> Either String Program
lexAndParse = tokenize >=> parse


typeCheck :: Program -> Either String (Program, TypeInfo)
typeCheck = buildTypeInformation

