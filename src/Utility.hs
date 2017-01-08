{-# LANGUAGE FlexibleContexts #-}
module Utility where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Data.List
import Data.Monoid

import AST
import Lexer
import CompileError


localState :: (MonadState s m) => (s -> s) -> m a -> m a
localState f m = do
    s <- get
    modify f
    r <- m
    put s
    return r

withState :: (MonadState s m) => s -> m a -> m a
withState s = localState (const s)

withPartialState :: (MonadState s m) => (s -> s') -> (s' -> s -> s) -> s' -> m a -> m a
withPartialState extract fuse s' m = do
    p <- gets extract
    modify $ fuse s'
    r <- m
    modify $ fuse p
    return r

withPartialPreservedState :: (MonadState s m) => (s -> s') -> (s' -> s -> s) -> m a -> m a
withPartialPreservedState extract fuse m = do
    p <- gets extract
    r <- m
    modify $ fuse p
    return r


findDuplicateOrd :: (Ord a) => [a] -> Maybe a
findDuplicateOrd [] = Nothing
findDuplicateOrd ls =
    let sorted = sort ls
        work a b
            | a == b    = First $ Just a
            | otherwise = First Nothing
    in getFirst $ mconcat $ zipWith work sorted $ tail sorted


whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing  _ = return ()
whenJust (Just a) f = f a


whenNothing :: (Monad m) => Maybe a -> m () -> m ()
whenNothing Nothing  m = m
whenNothing (Just _) _ = return ()


throwErrorRLoc :: (MonadError PhaseError m, MonadReader LocInfo m) => String -> m a
throwErrorRLoc s = do
    loc <- ask
    throwError $ PhaseError loc s

