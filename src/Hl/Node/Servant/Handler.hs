{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Hl.Node.Servant.Handler where

import           Control.Concurrent.Classy
import           Protolude                 hiding (MVar, modifyMVar_,
                                            tryReadMVar)
import           Servant.API               (NoContent (NoContent))


setVal'
  :: (MonadConc m)
  => Proxy m
  -> MVar m Text
  -> Text
  -> m NoContent
setVal' _ storage val = do
  modifyMVar_ storage (const $ pure val)
  pure NoContent

getVal'
  :: (MonadConc m)
  => Proxy m
  -> MVar m Text
  -> m (Maybe Text)
getVal' _ storage =
  tryReadMVar storage


