{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

-- |
--
--

module Fco.Backend.Actor (
    Request (..), Response (..), 
    spawnBackend) where

import BasicPrelude
import qualified Data.Text as T

import Data.IntMap (elems)

import Control.Concurrent.Actor (
    Channel, Message (..), MsgHandler, 
    defCtlHandler, send, spawnDefActor)
import Fco.Backend (query, storeTriple)
import Fco.Backend.Types (Environment)
import qualified Fco.Core.Types as CT


data Request = Query (Channel Response) CT.Query
             | Update CT.Triple

newtype Response = Response [CT.Triple]

spawnBackend :: Environment -> IO (Channel Request)
spawnBackend env = spawnDefActor backendHandler env

backendHandler :: MsgHandler Environment Request
backendHandler env (Message (Query rchannel qu)) = do
    tr <- query env qu
    send rchannel $ Message (Response tr)
    return $ Just env
backendHandler env (Message (Update tr)) = do
    storeTriple env tr
    return $ Just env
backendHandler env msg = defCtlHandler env msg

