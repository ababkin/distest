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


setVal
  :: forall m effs
  .  (MonadConc m, Member (TestEff m) effs, Member NodeEff effs)
  => Proxy m
  -> Text
  -> Eff effs ()
setVal _ val =
  setVal' val

getVal
  :: forall m effs
  .  (MonadConc m, Member (TestEff m) effs, Member NodeEff effs)
  => Proxy m
  -> Eff effs (Maybe Text)
getVal _ =
  getVal'


