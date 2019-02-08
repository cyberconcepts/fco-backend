{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- |
--
--

module Fco.Backend.Service (
    BackendRequest (..), BackendRespChannel, BackendResponse (..), 
    BackendService, 
    startBackendSvc,
    Request (..), Response (..), ResponseChan,
    setupBackend) where

import BasicPrelude
import qualified Data.Text as T

import Data.Binary (Binary)
import Data.IntMap (elems)
import GHC.Generics (Generic)

import Control.Distributed.Process (
    Process, ReceivePort, SendPort,
    newChan, sendChan, spawnLocal)

import Fco.Backend (query, storeTriple)
import Fco.Backend.Types (Environment)
import Fco.Core.Messaging (
    Channel, CtlChan, Message, MsgHandler, Notification (RequestQuit),
    ParentId, Service (..), ServiceId,
    defaultService, setupService,)
import qualified Fco.Core.Types as CT

import qualified Fco.Core.Service as Svc
import Fco.Core.Service (
    defaultCtlHandler, defaultListener, dummyHandler, startService)


-- new implementation, using Fco.Core.ServiceId

type BackendService = Svc.Service BackendRequest

type BackendRespChannel = Svc.Channel BackendResponse

data BackendRequest = BackendQuery BackendRespChannel CT.Query
                    | BackendUpdate CT.Triple

newtype BackendResponse = BackendResponse [CT.Triple]


startBackendSvc :: Environment -> IO BackendService
startBackendSvc env = startService defaultListener backendHandler env

backendHandler :: Svc.MsgHandler Environment BackendRequest
backendHandler env (Svc.Message (BackendQuery rchannel qu)) = do
    tr <- query env qu
    Svc.sendChan rchannel $ Svc.Message (BackendResponse tr)
    return $ Just env
backendHandler env (Svc.Message (BackendUpdate tr)) = do
    storeTriple env tr
    return $ Just env
backendHandler env msg = defaultCtlHandler env msg


-- legacy stuff, using distributed-process

data Request = Query (SendPort Response) CT.Query
                | Update CT.Triple
  deriving (Show, Generic, Typeable)
instance Binary Request
instance Message Request

data Response = Response ServiceId [CT.Triple]
  deriving (Show, Generic, Typeable)
instance Binary Response
instance Message Response

type ResponseChan = Channel Response


setupBackend :: ParentId -> Environment ->
                Process (SendPort Request, ServiceId)
setupBackend parent env = 
    let svc = defaultService { 
                messageHandler = reqHandler, 
                serviceState = env }
    in setupService svc parent


reqHandler :: MsgHandler Environment Request
reqHandler svcId env (Query client qu) = do
    tr <- liftIO $ query env qu
    sendChan client $ Response svcId tr
    return $ Just env
reqHandler svcId env (Update triple) = do
    liftIO $ storeTriple env triple
    return $ Just env
