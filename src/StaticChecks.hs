module StaticChecks where

import AST


staticChecks :: ProgramTyped -> Either String ()
staticChecks = checkReturns


checkReturns :: ProgramTyped -> Either String ()
checkReturns (Program fns cls) = mapM_ crFnDef fns


crFnDef :: FnDefTyped -> Either String ()
crFnDef (FnDef (TNamed "void") _ _ _) = Right ()
crFnDef (FnDef _ ident _ stmt) =
    if crStmt stmt
       then Right ()
       else Left $ "Not all control paths of function " ++ identifierLabel ident
                 ++ " return a value"


crStmt :: StmtTyped -> Bool
crStmt (Block _ stms) = any crStmt stms
crStmt (If _ s1 s2) = crStmt s1 && crStmt s2
crStmt (Return _) = True
crStmt VReturn = error "Void-like return in a non-void function!" -- It shouldn't happen!!!
crStmt _ = False

