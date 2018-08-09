{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module Hl.Test.Interpreter.MonadConc where

import           Control.Concurrent.Classy
import           Control.Monad.Freer
import           Hl.Test.Lang
import           Protolude                 hiding (modifyMVar_, newEmptyMVar,
                                            newMVar, newemptyMVar, readMVar,
                                            takeMVar, tryReadMVar, tryTakeMVar)


run
  :: forall m a . MonadConc m
  => Eff '[TestEff m, m] a
  -> m a
run = runM . interpretM (\case
        Fork          (p :: Proxy m) cont        -> fork cont
        ReadMVar      (p :: Proxy m) mv          -> readMVar mv
        TakeMVar      (p :: Proxy m) mv          -> takeMVar mv
        ModifyMVar_   (p :: Proxy m) mv modifier -> modifyMVar_ mv modifier
        NewMVar       (p :: Proxy m) val         -> newMVar val
        NewEmptyMVar  (p :: Proxy m)             -> newEmptyMVar
        TryTakeMVar   (p :: Proxy m) mv          -> tryTakeMVar mv
        TryReadMVar   (p :: Proxy m) mv          -> tryReadMVar mv
      )




