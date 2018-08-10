{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators  #-}

module Hl.Node.Servant.IO.Server where

{- import qualified Hl.Network.Interpreter.IO     as NetIpret -}
import qualified Hl.Node.Handler               as H
import qualified Hl.Node.Interpreter.Test      as NodeIpret
import           Hl.Node.Lang
import           Hl.Node.Servant.Api           (Routes (..))
import qualified Hl.Test.Interpreter.MonadConc as TestIpret
import           Protolude
import           Servant.API
import           Servant.Server
import           Servant.Server.Generic

{-
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
      netEnv <- NetEnv <$> newMVar empty
      liftIO $ TestIpret.run $ NetIpret.run netEnv $ NodeIpret.run ioProxy nodeEnv $ H.setVal' ioProxy storage val >> pure NoContent

    getVal
      :: Handler (Maybe Text)
    getVal = do
      netEnv <- NetEnv <$> newMVar empty
      liftIO $ TestIpret.run $ NetIpret.run netEnv $ NodeIpret.run ioProxy nodeEnv $ H.getVal' ioProxy storage

app :: NodeEnv IO -> Application
app = genericServe . server

-}


