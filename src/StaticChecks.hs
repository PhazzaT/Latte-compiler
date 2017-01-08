module StaticChecks where

import AST
import CompileError
import Lexer


staticChecks :: ProgramTyped -> Either PhaseError ()
staticChecks = checkReturns


checkReturns :: ProgramTyped -> Either PhaseError ()
checkReturns (Program fns cls) = mapM_ crFnDef fns


crFnDef :: FnDefTyped -> Either PhaseError ()
crFnDef (FnDef (TNamed "void") _ _ _ _) = Right ()
crFnDef (FnDef _ ident _ stmt loc) =
    if crStmt stmt
       then Right ()
       else Left $ PhaseError NoLocInfo $
                       "Not all control paths of function " ++ identifierLabel ident
                    ++ " return a value"


crStmt :: StmtTyped -> Bool
crStmt (Block _ stms) = any crStmt stms
crStmt (If _ s1 s2) = crStmt s1 && crStmt s2
crStmt (Return _) = True
crStmt VReturn = error "Void-like return in a non-void function!" -- It shouldn't happen!!!
crStmt _ = False

