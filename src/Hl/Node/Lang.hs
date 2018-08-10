{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Hl.Node.Lang where

import           Control.Concurrent.Classy
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
import           Core.Curry
import           Hl.Test.Lang
import           Protolude                  hiding (Chan, MVar, ThreadId)
import           System.Exit                hiding (ExitCode (ExitSuccess))


newtype NodeId = NodeId Int
  deriving (Eq, Ord, Num)
  deriving newtype Show

toPort :: NodeId -> Int
toPort (NodeId n) = 8080 + n


data TestReq =
    ReqSetVal Text
  | ReqGetVal
  deriving (Eq, Show)

data TestResp =
    RespSetVal
  | RespGetVal (Maybe Text)
  deriving (Eq, Show)



data Transport m = Transport {
    reqs  :: Chan m (TestReq, MVar m TestResp)
  }


data NodeEnv m = NodeEnv {
    nodeId    :: NodeId
  , storage   :: MVar m Text
  , transport :: Maybe (Transport m)
  }


data NodeEff r where
  SetVal      :: Text -> NodeEff ()
  GetVal      :: NodeEff (Maybe Text)

  -- this assumes success for now
  SetNodeVal  :: NodeId -> Text -> NodeEff ()
  GetNodeVal  :: NodeId -> NodeEff (Maybe Text)

setVal'
  :: (Member NodeEff effs)
  => Text
  -> Eff effs ()
setVal' = send . SetVal


getVal'
  :: (Member NodeEff effs)
  => Eff effs (Maybe Text)
getVal' = send GetVal


setNodeVal'
  :: (Member NodeEff effs)
  => NodeId
  -> Text
  -> Eff effs ()
setNodeVal' = send .: SetNodeVal

getNodeVal'
  :: (Member NodeEff effs)
  => NodeId
  -> Eff effs (Maybe Text)
getNodeVal' = send . GetNodeVal

