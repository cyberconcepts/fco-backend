{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- |
--
--

module Fco.Backend.Service (
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
