{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

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
    Mailbox, MsgHandler, StdBoxes,
    send, spawnStdActor)
import Fco.Backend (query, storeTriple)
import Fco.Backend.Types (Environment)
import qualified Fco.Core.Types as CT


data Request = Query (Mailbox Response) CT.Query
             | Update CT.Triple

newtype Response = Response [CT.Triple]

spawnBackend :: Environment -> IO (StdBoxes Request)
spawnBackend env = spawnStdActor backendHandler env

backendHandler :: MsgHandler Environment Request
backendHandler env (Query respbox qu) = do
    tr <- query env qu
    send respbox $ Response tr
    return $ Just env
backendHandler env (Update tr) = do
    storeTriple env tr
    return $ Just env

