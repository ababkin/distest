{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Hl.Network.Interpreter.IO where

import           Control.Concurrent.Classy
import           Control.Monad.Freer       hiding (run)
import           Data.Aeson
import qualified Data.Map                  as Map
import           Data.Proxy
import           GHC.Generics
import           Hl.Network.Lang
import qualified Hl.Node.Interpreter.Test  as NodeIpret
import           Hl.Node.Lang
import           Hl.Node.Servant.Api       (Routes (..))
import           Hl.Node.Servant.IO.Client (getVal, setVal)
import           Hl.Node.Servant.IO.Server (app)
import           Hl.Test.Lang
import           Network.HTTP.Client       (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp  as Warp (run)
import           Protolude                 hiding (MVar, modifyMVar_, newMVar)
import           Servant.Client


run
  :: forall a
  .  NetEnv IO
  -> (Eff '[NetEff, IO] a -> IO a)
run netEnv@NetEnv{ nodes } = runM . interpret (\case

  CallSetNodeVal callerNodeId targetNodeId payload -> liftIO $ do

    let val     = payload <> " by " <> show callerNodeId
        action  = setVal val

    mgr <- newManager defaultManagerSettings
    res <- runClientM action (mkClientEnv mgr (BaseUrl Http "localhost" (toPort targetNodeId) ""))
    case res of
      Left err ->
        putStrLn ("Error: " <> show err :: Text)
      Right _ ->
        putStrLn $ "value successfully set on node: " <> show targetNodeId <> ", " <> val

  CallGetNodeVal callerNodeId targetNodeId -> liftIO $ do

    let action = getVal

    mgr <- newManager defaultManagerSettings
    res <- runClientM action (mkClientEnv mgr (BaseUrl Http "localhost" (toPort targetNodeId) ""))
    case res of
      Left err -> do
        putStrLn ("Error: " <> show err :: Text)
        pure Nothing
      Right val -> do
        putStrLn ("got value: " <> show val <> " from node: " <> show targetNodeId :: Text)
        pure val

  StartNodeServer nodeId -> liftIO $ do
    storage <- newMVar "empty"

    threadId <- forkIO $
      Warp.run (toPort nodeId) (app storage)

    let transport = Nothing

    modifyMVar_ nodes $ pure . Map.insert nodeId NodeEnv{
        nodeId
      , storage
      , threadId
      , transport
      }

  )


