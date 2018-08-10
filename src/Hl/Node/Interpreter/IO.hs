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

module Hl.Node.Interpreter.IO where

import           Control.Concurrent.Classy
import           Control.Monad.Freer       hiding (run)
import           Data.Aeson
import qualified Data.Map                  as Map
import           Data.Proxy
import           GHC.Generics
import           Hl.Network.Lang
import qualified Hl.Node.Handler           as H
import           Hl.Node.Lang
import           Hl.Node.Servant.Api       (Routes (..))
import           Hl.Node.Servant.IO.Client (getVal, setVal)
import           Hl.Test.Lang
import           Network.HTTP.Client       (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp  as Warp (run)
import           Protolude                 hiding (MVar, modifyMVar_, newMVar,
                                            tryReadMVar)
import           Servant.API
import           Servant.Client
import           Servant.Server
import           Servant.Server.Generic


run
  :: forall a
  .  NodeEnv IO
  -> (Eff '[NodeEff, IO] a -> IO a)
run NodeEnv{ nodeId, storage } = runM . interpret (\case

  SetNodeVal targetNodeId payload -> liftIO $ do

    let val     = payload <> " by " <> show nodeId
        action  = setVal val

    mgr <- newManager defaultManagerSettings
    res <- runClientM action (mkClientEnv mgr (BaseUrl Http "localhost" (toPort targetNodeId) ""))
    case res of
      Left err ->
        putStrLn ("Error: " <> show err :: Text)
      Right _ ->
        putStrLn $ "value successfully set on node: " <> show targetNodeId <> ", " <> val

  GetNodeVal targetNodeId -> liftIO $ do

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

  SetVal val -> liftIO $ do
    modifyMVar_ storage (const $ pure val)

  GetVal -> liftIO $ do
    tryReadMVar storage

  )


ioProxy :: Proxy IO
ioProxy = Proxy

server
  :: NodeEnv IO
  -> Routes AsServer
server nodeEnv@NodeEnv{ storage } = Routes
  { _set = setVal
  , _get = getVal
  }

  where
    setVal
      :: Text
      -> Handler NoContent
    setVal val = do
      liftIO $ run nodeEnv $ H.setVal val
      pure NoContent

    getVal
      :: Handler (Maybe Text)
    getVal = do
      liftIO $ run nodeEnv $ H.getVal

app :: NodeEnv IO -> Application
app = genericServe . server

