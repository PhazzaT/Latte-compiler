module TypeCheck( buildTypeInformation
                , TypeInfo) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as M
import Data.Maybe

import AST
import Utility
import SpecialFunctions


type TypeCheckMonad = StateT Context (Except String)
data Context = Context { returnType             :: Type
                       , activeBlocks           :: [BlockID]
--                        , definitionLocationInfo :: DefinitionLocationInfo
                       , variableTypeInfo       :: TypeInfo
                       , functionTypeInfo       :: OverloadInfo
                       } deriving (Show, Eq)
-- (Type, BlockID, DefinitionLocationInfo)
-- type TypeInfo = M.Map VariableLocation Type
-- data VariableLocation = SLoc BlockID Identifier deriving (Eq, Ord, Show)
-- type DefinitionLocationInfo = M.Map Identifier BlockID
type TypeInfo = M.Map Identifier MangledIdentifier
type OverloadInfo = M.Map (Identifier, [Type]) Type


buildTypeInformation :: Program -> Either String ProgramTyped
buildTypeInformation = runTCM . btiProgram


btiProgram :: Program -> TypeCheckMonad ProgramTyped
btiProgram (Program fns) = do
    forM_ fns $ \(FnDef tRet ident args _) ->
        let argsTypes = map (\(Arg t _) -> t) args
        in writeFunctionInfo ident argsTypes tRet
    fmap Program $ forM fns $ \(FnDef tRet ident args body@(Block bid stms)) ->
        withBlock bid $ do
            let argsTypes = map (\(Arg t _) -> t) args
            args' <- forM args $ \(Arg t ident) -> do
                ident' <- writeVariableInfo ident t
                return $ Arg t ident'
            ident' <- mangleFunction ident argsTypes
            body' <- withReturnType tRet $ Block bid <$> mapM btiStmt stms
            return $ FnDef tRet ident' args' body'


btiStmt :: Stmt -> TypeCheckMonad StmtTyped
btiStmt (Assign e1 e2)         = do
    (e1', te1) <- btiExpr e1
    (e2', te2) <- btiExpr e2
    typeCompare te1 te2
    return $ Assign e1' e2'
btiStmt (Block bid stmts)      = withBlock bid $ Block bid <$> mapM btiStmt stmts
btiStmt (Decl t items)         = Decl t <$> mapM (btiItem t) items
btiStmt (Decr e)               = do
    (e', te) <- btiExpr e
    typeCompare tInt te
    return $ Decr e'
btiStmt Empty                  = return Empty
btiStmt (If cond s1 s2)        = do
    (cond', tcond) <- btiExpr cond
    typeCompare tBool tcond
    If cond' <$> btiStmt s1 <*> btiStmt s2
btiStmt (Incr e)               = do
    (e', te) <- btiExpr e
    typeCompare tInt te
    return $ Incr e'
btiStmt (Return e)             = do
    tRet <- gets returnType
    (e', te) <- btiExpr e
    typeCompare tRet te
    return $ Return e'
btiStmt (SExpr e)              = SExpr . fst <$> btiExpr e
btiStmt VReturn                = do
    tRet <- gets returnType
    typeCompare tVoid tRet
    return VReturn
btiStmt (While cond stmt)      = do
    (cond', tcond) <- btiExpr cond
    typeCompare tBool tcond
    While cond' <$> btiStmt stmt


btiItem :: Type -> Item -> TypeCheckMonad ItemTyped
btiItem t (Item ident me) = do
    e' <- case me of
        Just e  -> do
            (e'', te) <- btiExpr e
            typeCompare t te
            return $ Just e''
        Nothing -> return Nothing
    blocks <- gets activeBlocks
    ident' <- writeVariableInfo ident t
    return $ Item ident' e'


typeCompare :: Type -> Type -> TypeCheckMonad ()
typeCompare t1 t2 = when (t1 /= t2) $
    throwError $ "Expected type " ++ show t1 ++ ", got " ++ show t2


btiExpr :: Expr -> TypeCheckMonad (ExprTyped, Type)
btiExpr (EString s)           = return (EString s, tString)
btiExpr (EApp ident args)     = do
    (args', tArgs) <- unzip <$> mapM btiExpr args
    ident' <- mangleFunction ident tArgs
    let TFunction tRet _ = identifierType ident'
    return (EApp ident' args', tRet)
btiExpr (EBoolLiteral b)      = return (EBoolLiteral b, tBool)
btiExpr (EIntLiteral i)       = return (EIntLiteral i, tInt)
btiExpr (ENew t@(TArray _) [e]) = do
    (e', te) <- btiExpr e
    typeCompare tInt te
    return (ENew t [e'], t)
btiExpr (ENew _ _)            = throwError "Objects are not supported yet!"
btiExpr (EVar ident)          = do
    ident' <- mangleVariable ident
    return (EVar ident', identifierType ident')


writeVariableInfo :: Identifier -> Type -> TypeCheckMonad MangledIdentifier
writeVariableInfo ident t = do
    blocks <- gets activeBlocks
    oldDefinition <- gets $ M.lookup ident . variableTypeInfo
    let wasDefined = maybe False ((blocks==) . identifierScope) oldDefinition
    when wasDefined $ throwError $ "Variable " ++ show ident ++ " was already defined"
    let mangledIdent = MangledIdentifier ident t blocks
    modify $ \c -> c { variableTypeInfo = M.insert ident mangledIdent $ variableTypeInfo c }
    return mangledIdent


writeFunctionInfo :: Identifier -> [Type] -> Type -> TypeCheckMonad ()
writeFunctionInfo ident tArgs tRet = do
    wasDefined <- gets $ (Nothing/=) . M.lookup (ident, tArgs) . functionTypeInfo
    when wasDefined $ throwError $ "Multiple overloads of function " ++ show ident
                                    ++ " with the same arguments: " ++ show tArgs
    modify $ \c -> c { functionTypeInfo = M.insert (ident, tArgs) tRet $ functionTypeInfo c }


withReturnType :: Type -> TypeCheckMonad a -> TypeCheckMonad a
withReturnType = withPartialState returnType $ \t c -> c { returnType = t }


withBlock :: BlockID -> TypeCheckMonad a -> TypeCheckMonad a
withBlock bid m = do
    let extract c = (activeBlocks c, variableTypeInfo c)
    (blocks, vtInfo) <- gets extract
    withPartialState
        extract
        (\(b, vti) c -> c { activeBlocks = b, variableTypeInfo = vti })
        (bid : blocks, vtInfo)
        m


getVariableType :: Identifier -> TypeCheckMonad Type
getVariableType ident = identifierType <$> mangleVariable ident
    -- dli <- gets definitionLocationInfo
    -- info <- gets typeInfo
    -- case M.lookup ident dli >>= \loc -> M.lookup (SLoc loc ident) info of
    --     Just t  -> return t
    --     Nothing -> throwError $ "Unknown variable: " ++ show ident


runTCM :: TypeCheckMonad a -> Either String a
runTCM = runExcept . (`evalStateT` defaultContext)


defaultContext :: Context
defaultContext = Context { returnType       = tVoid
                         , activeBlocks     = [0]
                         , variableTypeInfo = defaultVariableTypeInfo
                         , functionTypeInfo = defaultFunctionTypeInfo}


defaultVariableTypeInfo = M.fromList []
defaultFunctionTypeInfo = M.fromList [ (("printInt", [tInt]), tVoid)
                                     , (("readInt", []), tInt)
                                     , (("printString", [tString]), tVoid)
                                     , (("readString", []), tString)
                                     ]


mangleVariable :: Identifier -> TypeCheckMonad MangledIdentifier
mangleVariable i = do
    vti <- gets variableTypeInfo
    case M.lookup i vti of
        Nothing -> throwError $ "Not in scope: variable " ++ show i
        Just mi -> return mi


mangleFunction :: Identifier -> [Type] -> TypeCheckMonad MangledIdentifier
mangleFunction i tArgs = do
    -- Check if it is a special function
    tRet <- do
        spec <- returnTypeOfSpecialFunction i tArgs
        case spec of
            Just t -> return t
            Nothing -> do
                fti <- gets functionTypeInfo
                case M.lookup (i, tArgs) fti of
                    Nothing   -> throwError $ "Unknown function: "
                                    ++ show i ++ " with args " ++ show tArgs
                    Just t    -> return t
    return $ MangledIdentifier i (TFunction tRet tArgs) [0]

