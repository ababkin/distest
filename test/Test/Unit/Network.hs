{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}


module Test.Unit.Network (spec) where

import           Control.Concurrent.Classy
import           Control.Monad.Freer
import qualified Data.Map                      as Map
import qualified Hl.Network.Interpreter.Test   as NetIpret
import           Hl.Network.Lang
import           Hl.Node.Lang                  (NodeEnv (..), NodeId)
import qualified Hl.Test.Interpreter.MonadConc as TestIpret
import           Hl.Test.Lang
import           Protolude                     hiding (newMVar)
import           Test.DejaFu                   (Predicate, ProPredicate (..),
                                                Result (..), autocheck, dejafu)
import           Test.Tasty.Hspec
import           Test.Util                     (assertNetworkResult, mkNetwork)


spec :: Spec
spec = parallel $ do
  describe "Network" $ do
    let createThreeNodes :: forall effs a . (Member NetEff effs) => Eff effs ()
        createThreeNodes = do
          startNodeServer' 1
          startNodeServer' 2
          startNodeServer' 3

        checkResults proxy env =
          let
            extractResults (nodeId, NodeEnv{ storage }) = (nodeId, ) <$> readMVar' proxy storage
          in
            traverse extractResults . Map.toList =<< readMVar' proxy (nodes env)



    it "registers nodes" $ do

      assertNetworkResult
        (mkNetwork createThreeNodes checkResults)
        [ (1, "empty")
        , (2, "empty")
        , (3, "empty")
        ]


    it "can call nodes from network level" $ do

      let callNodes :: forall effs a . (Member NetEff effs) => Eff effs ()
          callNodes = do
            createThreeNodes
            callSetNodeVal' 1 2 "touched by 1"
            val <- fromMaybe (panic "unexpected") <$> callGetNodeVal' 1 2
            callSetNodeVal' 2 3 $ val <> " by 2"
            pass

      assertNetworkResult
        (mkNetwork callNodes checkResults)
        [ (1, "empty")
        , (2, "touched by 1")
        , (3, "touched by 1 by 2")
        ]







