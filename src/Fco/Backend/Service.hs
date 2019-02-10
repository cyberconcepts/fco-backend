{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- |
--
--

module Fco.Backend.Service (
    Request (..), RespChannel, Response (..), 
    BackendService, 
    startBackendSvc) where

import BasicPrelude
import qualified Data.Text as T

import Data.IntMap (elems)

import Fco.Backend (query, storeTriple)
import Fco.Backend.Types (Environment)
import qualified Fco.Core.Types as CT
import Fco.Core.Service (
    Channel, Message (..), MsgHandler, Service,
    defaultCtlHandler, defaultListener, dummyHandler, sendChan, startService)


type BackendService = Service Request

type RespChannel = Channel Response

data Request = Query RespChannel CT.Query
             | Update CT.Triple

newtype Response = Response [CT.Triple]


startBackendSvc :: Environment -> IO BackendService
startBackendSvc env = startService defaultListener backendHandler env

backendHandler :: MsgHandler Environment Request
backendHandler env (Message (Query rchannel qu)) = do
    tr <- query env qu
    sendChan rchannel $ Message (Response tr)
    return $ Just env
backendHandler env (Message (Update tr)) = do
    storeTriple env tr
    return $ Just env
backendHandler env msg = defaultCtlHandler env msg

