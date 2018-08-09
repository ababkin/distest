{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Hl.Node.Handler where

import           Control.Concurrent.Classy
import           Protolude                 hiding (MVar, modifyMVar_,
                                            tryReadMVar)


setVal'
  :: (MonadConc m)
  => Proxy m
  -> MVar m Text
  -> Text
  -> m ()
setVal' _ storage val =
  modifyMVar_ storage (const $ pure val)

getVal'
  :: (MonadConc m)
  => Proxy m
  -> MVar m Text
  -> m (Maybe Text)
getVal' _ storage =
  tryReadMVar storage


