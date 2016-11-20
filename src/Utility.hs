module Utility where

import Control.Monad.State


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

