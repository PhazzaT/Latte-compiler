module CompilePasses ( compile
                     , lexAndParse
                     , typeCheck
                     , astToSSA
                     ) where

import Control.Monad((>=>))

import Data.Bifunctor
import Data.List
import qualified Data.Map as M

import AST
import ASTOptimizations
import Lexer
import Parser
import TypeCheck
-- import RenameUnique
import GenerateSSA
import CodeGen.Dumb
import CodeGen.AssemblyFormatters.Gas
import CodeGen.AssemblyFormatters.Nasm
import CompileError
import StaticChecks


compile :: String -> Either CompileError String
compile = lexAndParse
      >=> typeCheck
      >=> return . astOptimizations
      >=> staticAnalysis
      >=> generateAssembly
--       >=> astToSSA
--       >=> \m ->
--             let els = M.assocs m
--                 alles = flip map els $ \(name, stmts) ->
--                         show name ++ ":\n\t" ++ intercalate "\n\t" (map show stmts)
--             in return $ intercalate "\n\n" alles


lexAndParse :: String -> Either CompileError Program
lexAndParse = first parseError . (tokenize >=> parse)


typeCheck :: Program -> Either CompileError ProgramTyped
typeCheck = first typeCheckError . buildTypeInformation


staticAnalysis :: ProgramTyped -> Either CompileError ProgramTyped
staticAnalysis p = const p <$> first staticAnalysisError (checkReturns p)


generateAssembly :: ProgramTyped -> Either CompileError String
generateAssembly = first codeGenerationError . astToAsm gasFormatter

