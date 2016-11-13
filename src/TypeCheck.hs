module TypeCheck( buildTypeInformation
                , TypeInfo
                , DefinitionLocationInfo
                , SymbolLocation(SLoc)) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as M

import AST


type TypeCheckMonad = ReaderT (Type, BlockID, DefinitionLocationInfo)
                        (StateT TypeInfo
                            (Except String))
type TypeInfo = M.Map SymbolLocation Type
type DefinitionLocationInfo = M.Map Identifier BlockID
data SymbolLocation = SLoc BlockID String deriving (Eq, Ord, Show)


buildTypeInformation :: Program -> Either String (Program, TypeInfo)
buildTypeInformation p = runTCM $ btiProgram p >> return p


btiProgram :: Program -> TypeCheckMonad ()
btiProgram (Program fns) =
    -- TODO: Put function arguments in its scope
    -- TODO: Currently, it is assumed here that all functions contain blocks
    -- as their statement. It sounds reasonable, but the AST types could be
    -- changed in a way not to have to make this assumption.
    -- Collect function types
    foldr writeFunctionType typeCheckFunctionBodies fns
    where
        writeFunctionType (FnDef tRet ident args _) cont = do
            let argsTypes = map (\(Arg t _) -> t) args
            writeSymbolInfo ident (TFunction tRet argsTypes) cont
        typeCheckFunctionBodies = forM_ fns $ \(FnDef tRet _ args body@(Block bid stms)) ->
            withBlock bid $
                let contBlock = local (\(_, currBlock, dli) -> (tRet, currBlock, dli)) $
                        btiStmt body $ return ()
                    writeArgInfo (Arg t ident) = writeSymbolInfo ident t
                in foldr writeArgInfo contBlock args


btiStmt :: Stmt -> TypeCheckMonad () -> TypeCheckMonad ()
btiStmt (Assign ident e) cont  = do
    sType <- getSymbolType ident
    assertExprType sType e
    cont
btiStmt (Block bid stmts) cont = do
    withBlock bid $
        foldr btiStmt (return ()) stmts
    cont -- Do not put cont as an argument to foldr,
         -- because we do not want to preserve information
         -- from Reader part of the monad stack.
btiStmt (Decl t items) cont    = foldr (btiItem t) cont items
btiStmt Empty cont             = cont
btiStmt (If cond e1 e2) cont   = do
    assertExprType (TNamed "bool") cond
    btiStmt e1 $ btiStmt e2 cont
btiStmt (Return e) cont        = do
    tRet <- currentReturnType
    assertExprType tRet e
    cont
btiStmt (SExpr e) cont         = typeOfExpr e >> cont
btiStmt VReturn cont           = do
    tRet <- currentReturnType
    typeCompare (TNamed "void") tRet
    cont
btiStmt (While cond stmt) cont = do
    assertExprType (TNamed "bool") cond
    btiStmt stmt cont


btiItem :: Type -> Item -> TypeCheckMonad () -> TypeCheckMonad ()
btiItem t (ItemDeclDefault ident) cont = writeSymbolInfo ident t cont
btiItem t (ItemDecl ident e) cont = do
    assertExprType t e
    writeSymbolInfo ident t cont


typeCompare :: Type -> Type -> TypeCheckMonad ()
typeCompare t1 t2 = when (t1 /= t2) $
    throwError $ "Expected type " ++ show t1 ++ ", got " ++ show t2


typeOfExpr :: Expr -> TypeCheckMonad Type
typeOfExpr (ELogic e1 _ e2)   = assertExprTypePair (TNamed "bool") e1 e2
typeOfExpr (EAdd e1 Plus e2)  = do
    te1 <- typeOfExpr e1
    te2 <- typeOfExpr e2
    when (te1 /= te2 || (te1 /= TNamed "int" && te1 /= TNamed "string")) $
        throwError $ "Can't add together " ++ show te1 ++ " and " ++ show te2
    return te1
typeOfExpr (EAdd e1 Minus e2) = assertExprTypePair (TNamed "int") e1 e2
typeOfExpr (ERel e1 _ e2)     = assertExprTypePair (TNamed "int") e1 e2
    >> return (TNamed "bool")
typeOfExpr (EMul e1 _ e2)     = assertExprTypePair (TNamed "int") e1 e2
typeOfExpr (Not e)            = assertExprType (TNamed "int") e >> return (TNamed "int")
typeOfExpr (Neg e)            = assertExprType (TNamed "int") e >> return (TNamed "int")
typeOfExpr (EString _)        = return $ TNamed "string"
typeOfExpr (EApp f args) = do
    tf <- getSymbolType f
    case tf of
        TFunction tRet tArgs -> do
            let lSig = length tArgs
            let lCall = length args
            when (lSig /= lCall) $
                throwError $ "Function takes " ++ show lSig ++ " arguments, "
                          ++ "but " ++ show lCall ++ " are provided"
            forM_ (zip3 [1..] args tArgs) $ \(i, e, t) -> do
                te <- typeOfExpr e
                when (te /= t) $
                    throwError $ "Argument #" ++ show i ++ " should be of type "
                              ++ show t ++ ", but is of type " ++ show te
            return tRet
        _ -> throwError $ show f ++ " is not a function in this context, "
                                 ++ "it is of type " ++ show tf
typeOfExpr (EBoolLiteral _)   = return $ TNamed "bool"
typeOfExpr (EIntLiteral _)    = return $ TNamed "int"
typeOfExpr (EVar v)           = getSymbolType v


assertExprTypePair :: Type -> Expr -> Expr -> TypeCheckMonad Type
assertExprTypePair t e1 e2 = do
    assertExprType t e1
    assertExprType t e2
    return t


assertExprType :: Type -> Expr -> TypeCheckMonad ()
assertExprType t e = do
    te <- typeOfExpr e
    typeCompare t te


writeSymbolInfo :: Identifier -> Type -> TypeCheckMonad () -> TypeCheckMonad ()
writeSymbolInfo ident t cont = do
    -- TODO: Check if the variable was already defined in this block
    (tRet, currBlock, dli) <- ask
    wasDefined <- gets $ (Nothing/=) . M.lookup (SLoc currBlock ident)
    when wasDefined $ throwError $ "Symbol " ++ show ident ++ " was already defined"
    modify $ M.insert (SLoc currBlock ident) t
    local (const (tRet, currBlock, M.insert ident currBlock dli)) cont


withBlock :: BlockID -> TypeCheckMonad () -> TypeCheckMonad ()
withBlock bid = local (\(tRet, _, dli) -> (tRet, bid, dli)) 


getSymbolType :: Identifier -> TypeCheckMonad Type
getSymbolType ident = do
    (_, _, dli) <- ask
    info <- get
    case M.lookup ident dli >>= \loc -> M.lookup (SLoc loc ident) info of
        Just t  -> return t
        Nothing -> throwError $ "Unknown variable: " ++ show ident


currentReturnType :: TypeCheckMonad Type
currentReturnType = asks $ \(tRet, _, _) -> tRet


runTCM :: TypeCheckMonad a -> Either String (a, TypeInfo)
runTCM = runExcept . (`runStateT` M.empty) . (`runReaderT` (TNamed "void", 0, M.empty))

