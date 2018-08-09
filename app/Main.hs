{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Hl.Network.Interpreter.IO as NetworkIpret
import           Hl.Network.Lang
import           Protolude


main :: IO ()
main = do
  env <- NetEnv <$> newMVar mempty
  result <- NetworkIpret.run env $ do
    startNodeServer' 1
    startNodeServer' 2
    startNodeServer' 3
    callSetNodeVal' 1 2 "touched"
    callGetNodeVal' 1 2

  print ("node 2 state: " <> show result :: Text)
