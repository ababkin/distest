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
  :: forall effs
  .  (Member NodeEff effs)
  => Text
  -> Eff effs ()
setVal val =
  setVal' val

getVal
  :: forall effs
  .  (Member NodeEff effs)
  => Eff effs (Maybe Text)
getVal =
  getVal'


