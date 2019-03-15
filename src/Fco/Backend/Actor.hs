{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
-- Copyright   :  (C) 2019 team@functionalconcepts.org
-- License     :  MIT
-- Maintainer  :  Helmut Merz <helmutm@cy55.de>
-- Stability   :  experimental
-- Portability :  GHC only (requires STM)
--
-- Access to the FCO backend using the fco-actor framework.
--

module Fco.Backend.Actor (
    -- * Backend Actor
    Request (..), Response (..), 
    spawnBackend,
    -- * Usage Example
    demo
    ) where

import BasicPrelude hiding (lookup)
import qualified Data.Text as T

import Control.Monad.Extra (whileM)
import Data.IntMap (elems)

import Control.Concurrent.Actor (
    Actor,
    Behaviour (..), ControlMsg (..), Mailbox, MsgHandler, StdBoxes (..),
    call, messageBox, controlBox,
    defContext, defListener, mailbox, minimalContext,
    receiveMailbox, runActor, send, 
    spawnActor, spawnStdActor, stdBoxes, stdContext)
import Control.Concurrent.Actor.Config (
    ConfigRequest (..), ConfigResponse (..),
    spawnConfigDef)
import Control.Concurrent.Actor.Console (spawnConIn, spawnConOut)

import Fco.Backend (setupEnv, query, storeTriple)
import Fco.Backend.Types (
    Environment, 
    credentials, dbSettings, dbName, envDB, environment)
import qualified Fco.Core.Parse as CP
import qualified Fco.Core.Show as CS
import Fco.Core.Struct (lookup)
import qualified Fco.Core.Types as CT
import Fco.Core.Types (Namespace (..))


-- | A message used to query or update the backend.
data Request = Query CT.Query (Mailbox Response)
             | Update CT.Triple

-- | The response message type as returned (sent back) by the backend actor.
newtype Response = Response [CT.Triple]

-- | Start a backend actor 
spawnBackend ::  StdBoxes ConfigRequest -> IO (StdBoxes Request)
spawnBackend config = do
    ConfigResponse (_, cfg) <- call config (ConfigQuery "backend-pgsql")
    let db = dbSettings { dbName = lookup "dbname" cfg,
                          credentials = (lookup "dbuser" cfg, 
                                         lookup "dbpassword" cfg) }
    env <- setupEnv $ environment { envDB = db }
    spawnStdActor backendHandler env []

backendHandler :: MsgHandler Environment Request
backendHandler env (Query qu client) = do
    tr <- liftIO $ query env qu
    send client $ Response tr
    return $ Just env
backendHandler env (Update tr) = do
    liftIO $ storeTriple env tr
    return $ Just env


-- | An example main function that reads a query from stdin,
-- parses it, and queries the backend. 
-- The query result is printed to stdout.
--
-- Enter '? ? ?' to get a list of all triples.
demo :: IO ()
demo = do
    self <- stdBoxes
    respBox <- mailbox
    config <- spawnConfigDef
    backend <- spawnBackend config
    spawnConIn self
    output <- spawnConOut
    let selfCtx = defContext () [
            Behv (controlBox self) (ctlHandler config backend output),
            Behv (messageBox self) (inpHandler (messageBox backend) respBox),
            Behv respBox (responseHandler (messageBox output))
          ] []
    runActor defListener selfCtx

-- message handlers used by the demo function.

inpHandler :: Mailbox Request -> Mailbox Response -> MsgHandler st Text
inpHandler reqBox respBox state txt = do
    send reqBox $
            Query (CP.parseQuery (Namespace "") txt) respBox
    return $ Just state

ctlHandler :: StdBoxes ConfigRequest -> StdBoxes Request -> StdBoxes Text
            -> MsgHandler st ControlMsg
ctlHandler cfgBoxes reqBoxes outBoxes _ msg = do
    send (controlBox cfgBoxes) msg
    send (controlBox reqBoxes) msg
    send (controlBox outBoxes) msg
    return Nothing

responseHandler :: Mailbox Text -> MsgHandler st Response
responseHandler outbox state (Response triples) = do
    send outbox $ unlines (map CS.showTriple triples)
    return $ Just state

