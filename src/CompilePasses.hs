module CompilePasses ( compile
                     , lexAndParse
                     , typeCheck
                     , astToSSA
                     ) where

import Control.Monad((>=>))

import Data.List
import qualified Data.Map as M

import AST
import Lexer
import Parser
import TypeCheck
-- import RenameUnique
import GenerateSSA
import CodeGen.Dumb
import CodeGen.AssemblyFormatters.Nasm


compile :: String -> Either String String
compile = lexAndParse
      >=> typeCheck
      >=> astToAsm nasmFormatter
--       >=> astToSSA
--       >=> \m ->
--             let els = M.assocs m
--                 alles = flip map els $ \(name, stmts) ->
--                         show name ++ ":\n\t" ++ intercalate "\n\t" (map show stmts)
--             in return $ intercalate "\n\n" alles


lexAndParse :: String -> Either String Program
lexAndParse = tokenize >=> parse


typeCheck :: Program -> Either String ProgramTyped
typeCheck = buildTypeInformation

