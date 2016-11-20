{-# LANGUAGE FlexibleContexts #-}
module RenameUnique(renameUnique) where

import Control.Arrow(second)
import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import Data.List
import Data.Generics
import qualified Data.Map as M

import AST
import TypeCheck(TypeInfo, SymbolLocation(SLoc))
import Utility


type Mo = ExceptT String (State Context) -- (TypeInfo, BlockID, M.Map Identifier Identifier)
data Context = Context { typeInfo      :: TypeInfo
                       , blockID       :: BlockID
                       , substitutions :: M.Map Identifier Identifier
                       }


trampo :: (MonadError String m) => String -> m a -> m a
trampo s m = catchError m $ throwError . (++s)


renameUnique :: Program -> TypeInfo -> Either String Program
renameUnique (Program fns) ti = (`evalState` Context ti 0 M.empty) $ runExceptT $ do
    forM_ fns $ \(FnDef _ i _ _) -> introduceIdentifier i
    Program <$> mapM rename fns


introduceIdentifier :: Identifier -> Mo Identifier
introduceIdentifier i = do
    Context ti bid subs <- get
    i' <- case M.lookup (SLoc bid i) ti of
        Just t  -> return $ mangle i bid t
        Nothing -> throwError $ "(internal) Could not find type information for "
            ++ show i ++ ", " ++ show bid ++ ", " ++ show ti
            ++ "\n" ++ show ti
    put $ Context ti bid $ M.insert i i' subs
    return i'


rename :: (Data a, Typeable a) => a -> Mo a
rename = recur
            `extM` introduceDefinition
            `extM` introduceBlock
            `extM` introduceFunction
            `extM` renameIdentifier
    where
        introduceFunction :: FnDef -> Mo FnDef
        introduceFunction (FnDef tRet i args (Block bid stmts)) = trampo ("\nIn function " ++ show i) $
            localState (\c -> c { blockID = bid }) $ do
                args' <- forM args $ \(Arg t i) -> Arg t <$> introduceIdentifier i
                stmts' <- recur stmts
                FnDef tRet <$> renameIdentifier i <*> pure args' <*> pure (Block bid stmts')

        introduceDefinition :: Item -> Mo Item
        introduceDefinition (Item i me) = trampo "\nIn item" $ Item <$> introduceIdentifier i <*> pure me

        introduceBlock :: Stmt -> Mo Stmt
        introduceBlock (Block bid stmts) = trampo ("\nIn block " ++ show bid) $
            localState (\c -> c { blockID = bid }) $
                Block bid <$> recur stmts
        introduceBlock s = recur s

        renameIdentifier :: Identifier -> Mo Identifier
        renameIdentifier i = do
            subs <- gets substitutions
            case M.lookup i subs of
                Just i' -> return i'
                Nothing -> throwError $ "(internal) Could not find substitution for "
                    ++ show i ++ "\n" ++ show subs

        recur :: (Data a, Typeable a) => a -> Mo a
        recur = gmapM rename

