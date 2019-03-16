{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Nix.Fresh where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.ST
import Control.Monad.State.Strict
import Control.Monad.Writer
#ifdef MIN_VERSION_haskeline
import System.Console.Haskeline.MonadException hiding (catch)
#endif
import Nix.Var

-- TODO better fresh name supply
class Monad m => MonadFreshId i m | m -> i where
  freshId :: m i
  default freshId :: (MonadFreshId i m', MonadTrans t, m ~ (t m')) => m i
  freshId = lift freshId

newtype FreshIdT i m a = FreshIdT { unFreshIdT :: ReaderT (Var m i) m a }
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , MonadFix
    , MonadRef
    , MonadAtomicRef
    , MonadIO
    , MonadCatch
    , MonadThrow
#ifdef MIN_VERSION_haskeline
    , MonadException
#endif
    )

instance MonadTrans (FreshIdT i) where
    lift = FreshIdT . lift

instance MonadBase b m => MonadBase b (FreshIdT i m) where
    liftBase = FreshIdT . liftBase

-- instance MonadTransControl (FreshIdT i) where
--     type StT (FreshIdT i) a = StT (ReaderT (Var m i)) a
--     liftWith = defaultLiftWith FreshIdT unFreshIdT
--     restoreT = defaultRestoreT FreshIdT

-- instance MonadBaseControl b m => MonadBaseControl b (FreshIdT i m) where
--     type StM (FreshIdT i m) a = ComposeSt (FreshIdT i) m a
--     liftBaseWith     = defaultLiftBaseWith
--     restoreM         = defaultRestoreM

instance (MonadVar m, Num i) => MonadFreshId i (FreshIdT i m) where
  freshId = FreshIdT $ do
      v <- ask
      atomicModifyVar v (\i -> (i + 1, i))

runFreshIdT :: Functor m => Var m i -> FreshIdT i m a -> m a
runFreshIdT i m = runReaderT (unFreshIdT m) i

instance MonadFreshId i m => MonadFreshId i (ReaderT r m)
instance (Monoid w, MonadFreshId i m) => MonadFreshId i (WriterT w m)
instance MonadFreshId i m => MonadFreshId i (ExceptT e m)
instance MonadFreshId i m => MonadFreshId i (StateT s m)

-- Orphan instance needed by Infer.hs and Lint.hs

-- Since there's no forking, it's automatically atomic.
instance MonadAtomicRef (ST s) where
  atomicModifyRef r f = do
    v <- readRef r
    let (a, b) = f v
    writeRef r a
    return b
  atomicModifyRef' r f = do
    v <- readRef r
    let (a, b) = f v
    writeRef r $! a
    return b