{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- |
--
--

module Fco.Backend.Service (
    BackendRequest (..), BackendRespChannel, BackendResponse (..), 
    BackendService, 
    startBackendSvc) where

import BasicPrelude
import qualified Data.Text as T

import Data.IntMap (elems)

import Fco.Backend (query, storeTriple)
import Fco.Backend.Types (Environment)
import qualified Fco.Core.Types as CT

import Fco.Core.Service (
    Channel (..), Message (..), MsgHandler (..), Service (..),
    defaultCtlHandler, defaultListener, dummyHandler, sendChan, startService)


type BackendService = Service BackendRequest

type BackendRespChannel = Channel BackendResponse

data BackendRequest = BackendQuery BackendRespChannel CT.Query
                    | BackendUpdate CT.Triple

newtype BackendResponse = BackendResponse [CT.Triple]


startBackendSvc :: Environment -> IO BackendService
startBackendSvc env = startService defaultListener backendHandler env

backendHandler :: MsgHandler Environment BackendRequest
backendHandler env (Message (BackendQuery rchannel qu)) = do
    tr <- query env qu
    sendChan rchannel $ Message (BackendResponse tr)
    return $ Just env
backendHandler env (Message (BackendUpdate tr)) = do
    storeTriple env tr
    return $ Just env
backendHandler env msg = defaultCtlHandler env msg

