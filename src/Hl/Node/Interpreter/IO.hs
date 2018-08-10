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
import           Control.Monad.Freer           hiding (run)
import           Data.Aeson
import qualified Data.Map                      as Map
import           Data.Proxy
import           GHC.Generics
import           Hl.Network.Lang
import qualified Hl.Node.Handler               as H
import           Hl.Node.Lang
import           Hl.Node.Servant.Api           (Routes (..))
import           Hl.Node.Servant.IO.Client     (getVal, setVal)
import qualified Hl.Test.Interpreter.MonadConc as TestIpret
import           Hl.Test.Lang
import           Network.HTTP.Client           (defaultManagerSettings,
                                                newManager)
import qualified Network.Wai.Handler.Warp      as Warp (run)
import           Protolude                     hiding (MVar, modifyMVar_,
                                                newMVar, tryReadMVar)
import           Servant.API
import           Servant.Client
import           Servant.Server
import           Servant.Server.Generic


run
  :: forall a
  .  NodeEnv IO
  -> (Eff '[NodeEff, TestEff IO, IO] a -> Eff '[TestEff IO, IO] a)
run NodeEnv{ nodeId, storage } = interpret (\case

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

  SetVal val -> do
    modifyMVar_' ioProxy storage (const $ pure val)

  GetVal -> do
    tryReadMVar' ioProxy storage

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
      liftIO $ TestIpret.run $ run nodeEnv $ H.setVal ioProxy val
      pure NoContent

    getVal
      :: Handler (Maybe Text)
    getVal = do
      liftIO $ TestIpret.run $ run nodeEnv $ H.getVal ioProxy

app :: NodeEnv IO -> Application
app = genericServe . server
