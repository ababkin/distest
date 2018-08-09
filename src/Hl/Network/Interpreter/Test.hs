{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Hl.Network.Interpreter.Test where

import           Control.Concurrent.Classy
import           Control.Monad.Freer         hiding (run)
import qualified Data.Map                    as Map
import           Hl.Network.Lang
import qualified Hl.Node.Interpreter.Test    as NodeIpret
import           Hl.Node.Lang                hiding (GetVal, SetVal)
import           Hl.Node.Servant.Api         (api)
import qualified Hl.Node.Servant.Handler     as H
import           Hl.Node.Servant.Test.Client
import           Hl.Test.Lang
import           Protolude                   hiding (MVar)
import           Servant.API
import           Servant.Client.Core



run
  :: forall m effs a
  .  (MonadConc m, Member (TestEff m) effs, Member m effs)
  => Proxy m
  -> NetEnv m
  -> (Eff ( NetEff ': effs) a -> Eff effs a)
run proxy netEnv@NetEnv{ nodes } = interpret (\case
  CallSetNodeVal callerNodeId targetNodeId payload -> do
    nodes' <- readMVar' proxy nodes
    case Map.lookup targetNodeId nodes' of
      Just nodeEnv -> do
        -- this should be run in the node's server thread, not here
        -- run proxy netEnv $ NodeIpret.run proxy nodeEnv $ setVal' (payload <> " by " <> show callerNodeId)

        runTestClient nodeEnv $ setEndp payload
        pass

      Nothing ->
        pass

  CallGetNodeVal callerNodeId targetNodeId -> do
    nodes' <- readMVar' proxy nodes
    case Map.lookup targetNodeId nodes' of
      Just nodeEnv ->
        -- this should be run in the node's server thread, not here
        -- run proxy netEnv $ NodeIpret.run proxy nodeEnv getVal'

        runTestClient nodeEnv getEndp

      Nothing ->
        pure Nothing

  StartNodeServer nodeId -> do
    -- TODO
    storage <- newMVar' proxy "empty"

    threadId <- fork' proxy $
      {- (panic "server unimplemented" :: m ()) -}
      --
      -- asynchronously start the node's server
      pass

    modifyMVar_' proxy nodes $ pure . Map.insert nodeId (NodeEnv nodeId storage threadId)

  )

  where

    (setEndp :<|> getEndp) = api `clientIn` (Proxy :: Proxy (TestClient effs m))

