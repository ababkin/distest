{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Hl.Test.Lang where

import           Control.Concurrent.Classy
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
import           Core.Curry
import           Protolude                  hiding (MVar, ThreadId)


data TestEff m r where
  Fork          :: Proxy m -> m () -> TestEff m (ThreadId m)
  ModifyMVar_   :: Proxy m -> MVar m a -> (a -> m a) -> TestEff m ()
  ReadMVar      :: Proxy m -> MVar m a -> TestEff m a
  TakeMVar      :: Proxy m -> MVar m a -> TestEff m a
  NewMVar       :: Proxy m -> a -> TestEff m (MVar m a)
  NewEmptyMVar  :: Proxy m -> TestEff m (MVar m a)
  TryTakeMVar   :: Proxy m -> MVar m a -> TestEff m (Maybe a)
  TryReadMVar   :: Proxy m -> MVar m a -> TestEff m (Maybe a)

modifyMVar_'
  :: (Member (TestEff m) effs)
  => Proxy m
  -> MVar m a
  -> (a -> m a)
  -> Eff effs ()
modifyMVar_' = send .:: ModifyMVar_

takeMVar'
  :: (Member (TestEff m) effs)
  => Proxy m
  -> MVar m a
  -> Eff effs a
takeMVar' = send .: TakeMVar

readMVar'
  :: (Member (TestEff m) effs)
  => Proxy m
  -> MVar m a
  -> Eff effs a
readMVar' = send .: ReadMVar

newMVar'
  :: (Member (TestEff m) effs)
  => Proxy m
  -> a
  -> Eff effs (MVar m a)
newMVar' = send .: NewMVar

newEmptyMVar'
  :: (Member (TestEff m) effs)
  => Proxy m
  -> Eff effs (MVar m a)
newEmptyMVar' = send . NewEmptyMVar

tryTakeMVar'
  :: (Member (TestEff m) effs)
  => Proxy m
  -> MVar m a
  -> Eff effs (Maybe a)
tryTakeMVar' = send .: TryTakeMVar

tryReadMVar'
  :: (Member (TestEff m) effs)
  => Proxy m
  -> MVar m a
  -> Eff effs (Maybe a)
tryReadMVar' = send .: TryReadMVar

fork'
  :: (Member (TestEff m) effs)
  => Proxy m
  -> m ()
  -> Eff effs (ThreadId m)
fork' = send .: Fork

