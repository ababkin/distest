{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Hl.Node.Servant.Api where

import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Protolude
import           Servant.API
import           Servant.API.Generic
import           Servant.Links


api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy :: Proxy Routes)

data Routes route = Routes
  { _set :: route :- "set" :> ReqBody '[PlainText] Text  :> Put '[PlainText] NoContent
  , _get :: route :- "get"                               :> Get '[PlainText] (Maybe Text)
  }
  deriving (Generic)


setLink :: Link
setLink = fieldLink _set

getLink :: Link
getLink = fieldLink _get


instance MimeUnrender PlainText (Maybe Text) where
  mimeUnrender _ "" = pure Nothing
  mimeUnrender _ bs = purer $ toS bs

instance MimeRender PlainText (Maybe Text) where
  mimeRender _ (Just t) = toS t
  mimeRender _ Nothing  = ""
