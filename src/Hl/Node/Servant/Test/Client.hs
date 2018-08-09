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
{-# LANGUAGE ViewPatterns               #-}

module Hl.Node.Servant.Test.Client where

import           Control.Concurrent.Classy
import           Control.Monad.Freer
import           Data.Binary.Builder        hiding (empty)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Hl.Node.Lang               hiding (GetVal, SetVal)
import qualified Hl.Node.Servant.Handler    as H
import           Hl.Test.Lang
import           Network.HTTP.Types         (Status (..), hContentType, http11)
import           Protolude                  hiding (MVar)
import           Servant.Client.Core


newtype TestClient effs m a = TestClient{ unTestClient :: ReaderT (NodeEnv m) (Eff effs) a }
  deriving (Functor, Applicative, Monad, MonadReader (NodeEnv m))


runTestClient
  :: (MonadConc m, Member (TestEff m) effs)
  => NodeEnv m
  -> TestClient effs m a
  -> Eff effs a
runTestClient nodeEnv (TestClient action) =
  runReaderT action nodeEnv


-- TODO: below implementation should be substituted with a call to an MonadConc-abstract server

instance (MonadConc m, Member (TestEff m) effs, Member m effs) => RunClient (TestClient effs m) where
  runRequest Request{ requestPath = (toLazyByteString -> path) } | path == "/get" = do
    NodeEnv{ storage } <- ask
    val <- TestClient $ lift $ send $ H.getVal' (Proxy :: Proxy m) storage

    pure Response{
        responseStatusCode = Status 200 ""
      , responseHeaders = [(hContentType, "text/plain;charset=utf-8")]
      , responseHttpVersion = http11
      , responseBody = toS $ fromMaybe "" val
      }


  -- runRequest Request{ requestPath = (LBS8.split '/' . toLazyByteString -> ["", "set", payload]) } = do
  runRequest Request{ requestBody = Just ( RequestBodyLBS payload, _ )
                    , requestPath = (toLazyByteString -> path)
                    } | path == "/set" = do
    NodeEnv{ storage } <- ask
    TestClient $ lift $ send $ H.setVal' (Proxy :: Proxy m) storage $ toS payload

    pure Response{
        responseStatusCode = Status 200 ""
      , responseHeaders = [(hContentType, "text/plain;charset=utf-8")]
      , responseHttpVersion = http11
      , responseBody = ""
      }


  runRequest Request{ requestPath = (toLazyByteString -> path) } = panic $ "unknown path: " <> toS path

  streamingRequest = panic "no streaming implemented"
  throwServantError e = panic $ "throwServantError: " <> show e


