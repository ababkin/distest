{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Util where

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



mkNetwork
  :: forall m effs a . (MonadConc m)
  => (Eff '[NetEff, TestEff m, m] ())
  -> (Proxy m -> NetEnv m -> Eff '[TestEff m, m] a)
  -> m a
mkNetwork setupCont resultsCont = do
  TestIpret.run $ do
    let proxy = Proxy :: Proxy m

    env <- NetEnv <$> newMVar' proxy mempty
    NetIpret.run proxy env setupCont

    resultsCont proxy env


assertNetworkResult
  :: forall a
  .  (Eq a, Show a)
  => (forall m . (MonadConc m) => m a)
  -> a
  -> IO ()
assertNetworkResult action expectedResult = do
  autocheck action  `shouldReturn` True
  action            `shouldReturn` expectedResult

