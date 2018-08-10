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
import qualified Hl.Node.Handler             as H
import qualified Hl.Node.Interpreter.Test    as NodeIpret
import           Hl.Node.Lang                hiding (GetVal, SetVal)
import           Hl.Node.Servant.Api         (api)
import           Hl.Node.Servant.Test.Client
import           Hl.Test.Lang
import           Protolude                   hiding (Chan, MVar, putMVar,
                                              readChan, takeMVar)
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

        -- runTestClient nodeEnv $ setVal payload

        resp <- makeRequest nodeEnv $ ReqSetVal payload
        case resp of
          RespSetVal  -> pass
          unexpected  -> panic $ "received unexpected response: " <> show unexpected

      _ ->
        pass

  CallGetNodeVal callerNodeId targetNodeId -> do
    nodes' <- readMVar' proxy nodes
    case Map.lookup targetNodeId nodes' of
      Just nodeEnv -> do
        -- this should be run in the node's server thread, not here
        -- run proxy netEnv $ NodeIpret.run proxy nodeEnv getVal'

        -- runTestClient nodeEnv getVal

        resp <- makeRequest nodeEnv ReqGetVal
        case resp of
          RespGetVal val  -> pure val
          unexpected      -> panic $ "received unexpected response: " <> show unexpected

      _ ->
        pure Nothing

  StartNodeServer nodeId -> do
    storage   <- newMVar' proxy "empty"
    transport <- newTransport proxy

    fork' proxy $ forever $ do
      handleRequest transport $ \case
        ReqSetVal val -> H.setVal' proxy storage val  >>  pure RespSetVal
        ReqGetVal     -> H.getVal' proxy storage      >>= pure . RespGetVal

    modifyMVar_' proxy nodes $ pure . Map.insert nodeId NodeEnv{
        nodeId
      , storage
      , transport = Just transport
      }

  )

  where
    -- setVal :<|> getVal = api `clientIn` (Proxy :: Proxy (TestClient effs m))

    newTransport p = Transport <$> newChan' p

    makeRequest :: NodeEnv m -> TestReq -> Eff effs TestResp
    makeRequest NodeEnv{ transport = Just (Transport{ reqs })} req = do
      resp <- newEmptyMVar' proxy
      -- TODO: would be nice to mess with the order of requests in the pipeline
      writeChan' proxy reqs (req, resp)
      takeMVar' proxy resp
    makeRequest NodeEnv{ transport = Nothing } _ = panic "no transport"

    handleRequest :: Transport m -> (TestReq -> m TestResp) -> m ()
    handleRequest Transport{ reqs } handler = do
      (req, resp) <- readChan reqs
      putMVar resp =<< handler req
