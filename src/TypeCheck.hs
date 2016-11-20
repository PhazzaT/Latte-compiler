module TypeCheck( buildTypeInformation
                , TypeInfo
                , DefinitionLocationInfo
                , SymbolLocation(SLoc)) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as M
import Data.Maybe

import AST
import Utility


type TypeCheckMonad = StateT Context (Except String)
data Context = Context { returnType             :: Type
                       , blockID                :: BlockID
                       , definitionLocationInfo :: DefinitionLocationInfo
                       , typeInfo               :: TypeInfo
                       } deriving (Show, Eq)
-- (Type, BlockID, DefinitionLocationInfo)
type TypeInfo = M.Map SymbolLocation Type
data SymbolLocation = SLoc BlockID Identifier deriving (Eq, Ord, Show)
type DefinitionLocationInfo = M.Map Identifier BlockID


buildTypeInformation :: Program -> Either String (Program, TypeInfo)
buildTypeInformation p = runTCM $ btiProgram p >> return p


btiProgram :: Program -> TypeCheckMonad ()
btiProgram (Program fns) =
    -- TODO: Put function arguments in its scope
    -- TODO: Currently, it is assumed here that all functions contain blocks
    -- as their statement. It sounds reasonable, but the AST types could be
    -- changed in a way not to have to make this assumption.
    -- Collect function types
    mapM_ writeFunctionType fns >> typeCheckFunctionBodies
    where
        writeFunctionType (FnDef tRet ident args _) = do
            let argsTypes = map (\(Arg t _) -> t) args
            writeSymbolInfo ident (TFunction tRet argsTypes)
        typeCheckFunctionBodies = forM_ fns $ \(FnDef tRet _ args body@(Block bid stms)) ->
            withBlock bid $ do
                forM_ args $ \(Arg t ident) -> writeSymbolInfo ident t
                withReturnType tRet $ btiStmt body


btiStmt :: Stmt -> TypeCheckMonad ()
btiStmt (Assign ident e)       = do
    sType <- getSymbolType ident
    assertExprType sType e
btiStmt (Block bid stmts)      = withBlock bid $ mapM_ btiStmt stmts
btiStmt (Decl t items)         = mapM_ (btiItem t) items
btiStmt Empty                  = return ()
btiStmt (If cond e1 e2)        = do
    assertExprType (TNamed "bool") cond
    btiStmt e1 >> btiStmt e2
btiStmt (Return e)             = do
    tRet <- gets returnType
    assertExprType tRet e
btiStmt (SExpr e)              = void $ typeOfExpr e
btiStmt VReturn                = do
    tRet <- gets returnType
    typeCompare (TNamed "void") tRet
btiStmt (While cond stmt)      = do
    assertExprType (TNamed "bool") cond
    btiStmt stmt


btiItem :: Type -> Item -> TypeCheckMonad ()
btiItem t (Item ident me) = do
    fromMaybe (return ()) $ assertExprType t <$> me
    writeSymbolInfo ident t


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


writeSymbolInfo :: Identifier -> Type -> TypeCheckMonad ()
writeSymbolInfo ident t = do
    -- TODO: Check if the variable was already defined in this block
    -- (tRet, currBlock, dli) <- ask
    currBlock <- gets blockID
    wasDefined <- gets $ (Nothing/=) . M.lookup (SLoc currBlock ident) . typeInfo
    when wasDefined $ throwError $ "Symbol " ++ show ident ++ " was already defined"
    modify $ \c -> c { typeInfo = M.insert (SLoc currBlock ident) t $ typeInfo c
                     , definitionLocationInfo = M.insert ident currBlock $ definitionLocationInfo c }


withReturnType :: Type -> TypeCheckMonad a -> TypeCheckMonad a
withReturnType = withPartialState returnType $ \t c -> c { returnType = t }


withBlock :: BlockID -> TypeCheckMonad a -> TypeCheckMonad a
withBlock = withPartialState blockID $ \b c -> c { blockID = b }


withDefinitionLocationInfo :: DefinitionLocationInfo -> TypeCheckMonad a -> TypeCheckMonad a
withDefinitionLocationInfo = withPartialState definitionLocationInfo $ \d c -> c { definitionLocationInfo = d }


getSymbolType :: Identifier -> TypeCheckMonad Type
getSymbolType ident = do
    dli <- gets definitionLocationInfo
    info <- gets typeInfo
    case M.lookup ident dli >>= \loc -> M.lookup (SLoc loc ident) info of
        Just t  -> return t
        Nothing -> throwError $ "Unknown variable: " ++ show ident


runTCM :: TypeCheckMonad a -> Either String (a, TypeInfo)
runTCM m = runExcept . (`evalStateT` defaultContext) $ (,) <$> m <*> gets typeInfo


defaultContext :: Context
defaultContext = Context { returnType             = TNamed "void"
                         , blockID                = 0
                         , definitionLocationInfo = M.empty
                         , typeInfo               = M.empty
                         }


tVoid :: Type
tVoid = TNamed "void"

tBool :: Type
tBool = TNamed "bool"

tString :: Type
tString = TNamed "string"

tInt :: Type
tInt = TNamed "int"

