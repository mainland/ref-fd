-- |
-- Module      :  Control.Monad.Ref
-- Copyright   :  (c) Harvard University 2006-2011
--                (c) Geoffrey Mainland 2011-2012
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
-- Stability   :  experimental
-- Portability :  non-portable
--
--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Ref (
    MonadRef(..),
    MonadAtomicRef(..)
  ) where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar (TVar,
                                    newTVar,
                                    readTVar,
                                    writeTVar)
import Control.Monad.ST (ST)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Error (ErrorT, Error)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Lazy as Lazy (StateT)
import Control.Monad.Trans.State.Strict as Strict (StateT)
import Control.Monad.Trans.Writer.Lazy as Lazy (WriterT)
import Control.Monad.Trans.Writer.Strict as Strict (WriterT)
import Control.Monad.Trans.Class (lift)
import Data.IORef (IORef,
                   atomicModifyIORef,
                   modifyIORef,
                   newIORef,
                   readIORef,
                   writeIORef)
import Data.Monoid (Monoid)
import Data.STRef (STRef,
                   modifySTRef,
                   newSTRef,
                   readSTRef,
                   writeSTRef)

-- |The 'MonadRef' type class abstracts over the details of manipulating
-- references, allowing one to write code that uses references and can operate
-- in any monad that supports reference operations.

class (Monad m) => MonadRef r m | m -> r where
    -- |Create a new reference
    newRef    :: a -> m (r a)
    -- |Read the value of a reference
    readRef   :: r a -> m a
    -- |Write a new value to a reference
    writeRef  :: r a -> a -> m ()
    -- |Mutate the contents of a reference
    modifyRef :: r a -> (a -> a) -> m ()
    modifyRef r f = readRef r >>= writeRef r . f

class (MonadRef r m) => MonadAtomicRef r m | m -> r where
    -- |Atomically mutate the contents of a reference
    atomicModifyRef :: r a -> (a -> (a, b)) -> m b

instance MonadRef (STRef s) (ST s) where
    newRef    = newSTRef
    readRef   = readSTRef
    writeRef  = writeSTRef
    modifyRef = modifySTRef

instance MonadRef IORef IO where
    newRef    = newIORef
    readRef   = readIORef
    writeRef  = writeIORef
    modifyRef = modifyIORef

instance MonadRef TVar STM where
    newRef    = newTVar
    readRef   = readTVar
    writeRef  = writeTVar

instance MonadRef r m => MonadRef r (ContT r' m) where
    newRef    r   = lift $ newRef    r
    readRef   r   = lift $ readRef   r
    writeRef  r x = lift $ writeRef  r x
    modifyRef r f = lift $ modifyRef r f

instance (Error e, MonadRef r m) => MonadRef r (ErrorT e m) where
    newRef    r   = lift $ newRef    r
    readRef   r   = lift $ readRef   r
    writeRef  r x = lift $ writeRef  r x
    modifyRef r f = lift $ modifyRef r f

instance MonadRef r m => MonadRef r (IdentityT m) where
    newRef    r   = lift $ newRef    r
    readRef   r   = lift $ readRef   r
    writeRef  r x = lift $ writeRef  r x
    modifyRef r f = lift $ modifyRef r f

instance MonadRef r m => MonadRef r (ListT m) where
    newRef    r   = lift $ newRef    r
    readRef   r   = lift $ readRef   r
    writeRef  r x = lift $ writeRef  r x
    modifyRef r f = lift $ modifyRef r f

instance MonadRef r m => MonadRef r (MaybeT m) where
    newRef    r   = lift $ newRef    r
    readRef   r   = lift $ readRef   r
    writeRef  r x = lift $ writeRef  r x
    modifyRef r f = lift $ modifyRef r f

instance MonadRef r m => MonadRef r (ReaderT r' m) where
    newRef    r   = lift $ newRef    r
    readRef   r   = lift $ readRef   r
    writeRef  r x = lift $ writeRef  r x
    modifyRef r f = lift $ modifyRef r f

instance MonadRef r m => MonadRef r (Lazy.StateT s m) where
    newRef    r   = lift $ newRef    r
    readRef   r   = lift $ readRef   r
    writeRef  r x = lift $ writeRef  r x
    modifyRef r f = lift $ modifyRef r f

instance MonadRef r m => MonadRef r (Strict.StateT s m) where
    newRef    r   = lift $ newRef    r
    readRef   r   = lift $ readRef   r
    writeRef  r x = lift $ writeRef  r x
    modifyRef r f = lift $ modifyRef r f

instance (Monoid w, MonadRef r m) => MonadRef r (Lazy.WriterT w m) where
    newRef    r   = lift $ newRef    r
    readRef   r   = lift $ readRef   r
    writeRef  r x = lift $ writeRef  r x
    modifyRef r f = lift $ modifyRef r f

instance (Monoid w, MonadRef r m) => MonadRef r (Strict.WriterT w m) where
    newRef    r   = lift $ newRef    r
    readRef   r   = lift $ readRef   r
    writeRef  r x = lift $ writeRef  r x
    modifyRef r f = lift $ modifyRef r f

instance MonadAtomicRef IORef IO where
    atomicModifyRef = atomicModifyIORef

instance MonadAtomicRef TVar STM where
    atomicModifyRef r f = do x <- readRef r
                             let (x', y) = f x
                             writeRef r x'
                             return y

instance MonadAtomicRef r m => MonadAtomicRef r (ContT r' m) where
    atomicModifyRef r f = lift $ atomicModifyRef r f

instance (Error e, MonadAtomicRef r m) => MonadAtomicRef r (ErrorT e m) where
    atomicModifyRef r f = lift $ atomicModifyRef r f

instance MonadAtomicRef r m => MonadAtomicRef r (IdentityT m) where
    atomicModifyRef r f = lift $ atomicModifyRef r f

instance MonadAtomicRef r m => MonadAtomicRef r (ListT m) where
    atomicModifyRef r f = lift $ atomicModifyRef r f

instance MonadAtomicRef r m => MonadAtomicRef r (MaybeT m) where
    atomicModifyRef r f = lift $ atomicModifyRef r f

instance MonadAtomicRef r m => MonadAtomicRef r (ReaderT r' m) where
    atomicModifyRef r f = lift $ atomicModifyRef r f

instance MonadAtomicRef r m => MonadAtomicRef r (Lazy.StateT s m) where
    atomicModifyRef r f = lift $ atomicModifyRef r f

instance MonadAtomicRef r m => MonadAtomicRef r (Strict.StateT s m) where
    atomicModifyRef r f = lift $ atomicModifyRef r f

instance (Monoid w, MonadAtomicRef r m) => MonadAtomicRef r (Lazy.WriterT w m) where
    atomicModifyRef r f = lift $ atomicModifyRef r f

instance (Monoid w, MonadAtomicRef r m) => MonadAtomicRef r (Strict.WriterT w m) where
    atomicModifyRef r f = lift $ atomicModifyRef r f
