{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}

module Hl.Node.Handler where

import           Control.Concurrent.Classy
import           Control.Monad.Freer
import           Hl.Node.Lang
import           Hl.Test.Lang
import           Protolude                 hiding (MVar, modifyMVar_,
                                            tryReadMVar)


setVal'
  :: forall m effs a
  .  (MonadConc m, Member NodeEff effs, Member (TestEff m) effs)
  => Proxy m
  -> MVar m Text
  -> Text
  -> Eff effs ()
setVal' proxy storage val =
  modifyMVar_' proxy storage (const $ pure val)

getVal'
  :: forall m effs a
  .  (MonadConc m, Member NodeEff effs, Member (TestEff m) effs)
  => Proxy m
  -> MVar m Text
  -> Eff effs (Maybe Text)
getVal' proxy storage =
  tryReadMVar' proxy storage


