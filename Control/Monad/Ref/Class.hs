-- Copyright (c) 2006-2011
--         The President and Fellows of Harvard College.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the University nor the names of its contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

--------------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Ref
-- Copyright   :  (c) Harvard University 2006-2011
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
-- Stability   :  experimental
-- Portability :  non-portable
--
--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_DefaultSignatures
{-# LANGUAGE DefaultSignatures #-}
#endif
{-# LANGUAGE
    FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses
  , UndecidableInstances #-}
module Control.Monad.Ref.Class
       ( MonadRef (..)
       , modifyRefDefault
       , MonadAtomicRef (..)
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
import Control.Monad.Trans.Class (MonadTrans (lift))
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

class Monad m => MonadRef ref m | m -> ref where
    -- |Create a new reference
    newRef :: a -> m (ref a)
#ifdef LANGUAGE_DefaultSignatures
    default newRef :: (MonadRef ref m, MonadTrans t) => a -> t m (ref a)
    newRef = lift . newRef
#endif
    -- | Read the value of a reference
    readRef :: ref a -> m a
#ifdef LANGUAGE_DefaultSignatures
    default readRef :: (MonadRef ref m, MonadTrans t) => ref a -> t m a
    readRef = lift . readRef
#endif
    -- | Write a new value to a reference
    writeRef :: ref a -> a -> m ()
#ifdef LANGUAGE_DefaultSignatures
    default writeRef :: (MonadRef ref m, MonadTrans t) => ref a -> a -> t m ()
    writeRef ref = lift . writeRef ref
#endif
    -- | Mutate the contents of a reference
    modifyRef :: ref a -> (a -> a) -> m ()
    modifyRef r f = readRef r >>= writeRef r . f

modifyRefDefault :: ( MonadRef ref m
                    , MonadTrans t
                    ) => ref a -> (a -> a) -> t m ()
modifyRefDefault ref = lift . modifyRef ref

class MonadRef ref m => MonadAtomicRef ref m where
    -- | Atomically mutate the contents of a reference
    atomicModifyRef :: ref a -> (a -> (a, b)) -> m b
#ifdef LANGUAGE_DefaultSignatures
    default atomicModifyRef :: ( MonadAtomicRef ref m
                               , MonadTrans t
                               ) => ref a -> (a -> (a, b)) -> t m b
    atomicModifyRef ref = lift . atomicModifyRef ref
#endif

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

#ifndef LANGUAGE_DefaultSignatures
#define ModifyRef_defaults\
  newRef = lift . newRef;\
  readRef = lift . readRef;\
  writeRef ref = lift . writeRef ref;\
  modifyRef = modifyRefDefault
#else
#define ModifyRef_defaults\
  modifyRef = modifyRefDefault
#endif

instance MonadRef ref m => MonadRef ref (ContT r m) where
  ModifyRef_defaults
instance (Error e, MonadRef ref m) => MonadRef ref (ErrorT e m) where
  ModifyRef_defaults
instance MonadRef ref m => MonadRef ref (IdentityT m) where
  ModifyRef_defaults
instance MonadRef ref m => MonadRef ref (ListT m) where
  ModifyRef_defaults
instance MonadRef ref m => MonadRef ref (MaybeT m) where
  ModifyRef_defaults
instance MonadRef ref m => MonadRef ref (ReaderT r m) where
  ModifyRef_defaults
instance MonadRef ref m => MonadRef ref (Lazy.StateT s m) where
  ModifyRef_defaults
instance MonadRef ref m => MonadRef ref (Strict.StateT s m) where
  ModifyRef_defaults
instance (Monoid w, MonadRef ref m) => MonadRef ref (Lazy.WriterT w m) where
  ModifyRef_defaults
instance (Monoid w, MonadRef ref m) => MonadRef ref (Strict.WriterT w m) where
  ModifyRef_defaults

instance MonadAtomicRef IORef IO where
    atomicModifyRef = atomicModifyIORef

instance MonadAtomicRef TVar STM where
    atomicModifyRef r f = do x <- readRef r
                             let (x', y) = f x
                             writeRef r x'
                             return y

#ifndef LANGUAGE_DefaultSignatures
#define MonadAtomicRef_defaults\
  atomicModifyRef ref = lift . atomicModifyRef ref
#else
#define MonadAtomicRef_defaults
#endif

instance MonadAtomicRef ref m => MonadAtomicRef ref (ContT r m) where
  MonadAtomicRef_defaults
instance ( Error e
         , MonadAtomicRef ref m
         ) => MonadAtomicRef ref (ErrorT e m) where
  MonadAtomicRef_defaults
instance MonadAtomicRef ref m => MonadAtomicRef ref (IdentityT m) where
  MonadAtomicRef_defaults
instance MonadAtomicRef ref m => MonadAtomicRef ref (ListT m) where
  MonadAtomicRef_defaults
instance MonadAtomicRef ref m => MonadAtomicRef ref (MaybeT m) where
  MonadAtomicRef_defaults
instance MonadAtomicRef ref m => MonadAtomicRef ref (ReaderT r m) where
  MonadAtomicRef_defaults
instance MonadAtomicRef ref m => MonadAtomicRef ref (Lazy.StateT s m) where
  MonadAtomicRef_defaults
instance MonadAtomicRef ref m => MonadAtomicRef ref (Strict.StateT s m) where
  MonadAtomicRef_defaults
instance ( Monoid w
         , MonadAtomicRef ref m
         ) => MonadAtomicRef ref (Lazy.WriterT w m) where
  MonadAtomicRef_defaults
instance ( Monoid w
         , MonadAtomicRef ref m
         ) => MonadAtomicRef ref (Strict.WriterT w m) where
  MonadAtomicRef_defaults
