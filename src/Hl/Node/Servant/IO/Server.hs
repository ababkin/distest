{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Hl.Node.Servant.IO.Server where

import           Hl.Node.Handler        (getVal', setVal')
import           Hl.Node.Servant.Api    (Routes (..))
import           Protolude
import           Servant.API
import           Servant.Server
import           Servant.Server.Generic


ioProxy :: Proxy IO
ioProxy = Proxy

server
  :: MVar Text
  -> Routes AsServer
server storage = Routes
  { _set = setVal storage
  , _get = getVal storage
  }

setVal
  :: MVar Text
  -> Text
  -> Handler NoContent
setVal storage val =
  liftIO $ setVal' ioProxy storage val >> pure NoContent

getVal
  :: MVar Text
  -> Handler (Maybe Text)
getVal storage =
  liftIO $ getVal' ioProxy storage

app :: MVar Text -> Application
app = genericServe . server




