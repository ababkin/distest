{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Hl.Node.Servant.IO.Client where

import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Hl.Node.Servant.Api    (api)
import           Protolude
import           Servant.API
import           Servant.Client
import           Servant.Client.Generic


setVal
  :: Text -> ClientM NoContent

getVal
  :: ClientM (Maybe Text)


setVal :<|> getVal = client api

-- | Simple data type to represent the target of HTTP requests
--   for servant's automatically-generated clients.
data BaseUrl = BaseUrl
  { baseUrlScheme :: Scheme -- ^ URI scheme to use
  , baseUrlHost   :: [Char]   -- ^ host (eg "haskell.org")
  , baseUrlPort   :: Int      -- ^ port (eg 80)
  , baseUrlPath   :: [Char]   -- ^ path (eg "/a/b/c")
  }


