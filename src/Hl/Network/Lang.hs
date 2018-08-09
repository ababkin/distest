{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}

module Hl.Network.Lang where

import           Control.Concurrent.Classy
import           Control.Monad.Freer
import           Core.Curry
import           Hl.Node.Lang              (NodeEnv, NodeId)
import           Hl.Test.Lang
import           Protolude                 hiding (MVar)


data NetEnv m = NetEnv {
    nodes :: MVar m ( Map NodeId (NodeEnv m) )
  }

data NetEff r where
  CallGetNodeVal  :: NodeId -> NodeId -> NetEff (Maybe Text)
  CallSetNodeVal  :: NodeId -> NodeId -> Text -> NetEff ()
  StartNodeServer :: NodeId -> NetEff ()

callSetNodeVal'
  :: (Member NetEff effs)
  => NodeId
  -> NodeId
  -> Text
  -> Eff effs ()
callSetNodeVal' = send .:: CallSetNodeVal

callGetNodeVal'
  :: (Member NetEff effs)
  => NodeId
  -> NodeId
  -> Eff effs (Maybe Text)
callGetNodeVal' = send .: CallGetNodeVal

startNodeServer'
  :: (Member NetEff effs)
  => NodeId
  -> Eff effs ()
startNodeServer' = send . StartNodeServer

