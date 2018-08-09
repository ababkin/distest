{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Hl.Node.Interpreter.Test where

import           Control.Concurrent.Classy
import           Control.Monad.Freer
import           Hl.Network.Lang
import           Hl.Node.Lang
import           Hl.Test.Lang
import           Protolude                 hiding (MVar)


run
  :: (MonadConc m, Member (TestEff m) effs, Member NetEff effs)
  => Proxy m
  -> NodeEnv m
  -> (Eff ( NodeEff ': effs) a -> Eff effs a)
run proxy NodeEnv{ nodeId, storage } = interpret (\case

  SetVal val  ->
    modifyMVar_' proxy storage (const $ pure val)

  GetVal      ->
    tryReadMVar' proxy storage

  SetNodeVal targetNodeId payload ->
    callSetNodeVal' nodeId targetNodeId payload

  GetNodeVal targetNodeId ->
    callGetNodeVal' nodeId targetNodeId

  )

