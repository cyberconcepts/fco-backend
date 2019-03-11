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

import BasicPrelude
import qualified Data.Text as T

import Control.Monad.Extra (whileM)
import Data.IntMap (elems)

import Control.Concurrent.Actor (
    Actor,
    Behaviour (..), ControlMsg (..), Mailbox, MsgHandler, StdBoxes (..),
    messageBox, controlBox,
    defContext, defListener, mailbox, runActor, send, 
    spawnActor, spawnStdActor, stdBoxes, stdContext)
import Control.Concurrent.Actor.Config (spawnConfigDef)
import Control.Concurrent.Actor.Console (spawnConIn, spawnConOut)

import Fco.Backend (setupEnv, query, storeTriple)
import Fco.Backend.Types (Environment, dbSettings, dbName, envDB, environment)
import qualified Fco.Core.Parse as CP
import qualified Fco.Core.Show as CS
import qualified Fco.Core.Types as CT
import Fco.Core.Types (Namespace (..))


-- | A message used to query or update the backend.
data Request = Query (Mailbox Response) CT.Query
             | Update CT.Triple

-- | The response message type as returned (sent back) by the backend actor.
newtype Response = Response [CT.Triple]

-- | Start a backend actor 
spawnBackend ::  Environment -> IO (StdBoxes Request)
spawnBackend env =
    spawnStdActor backendHandler env []

backendHandler :: MsgHandler Environment Request
backendHandler env (Query client qu) = do
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
    config <- spawnConfigDef -- not used yet
    let db = dbSettings { dbName = "fco_test" }
    env <- liftIO $ setupEnv $ environment { envDB = db }
    backend <- spawnBackend env -- TODO: use config
    spawnConIn self
    output <- spawnConOut
    let selfCtx = defContext () [
            Behv (controlBox self) (ctlHandler output backend),
            Behv (messageBox self) (inpHandler (messageBox backend) respBox),
            Behv respBox (responseHandler (messageBox output))
          ] []
    runActor defListener selfCtx

-- message handlers used by the demo function.

inpHandler :: Mailbox Request -> Mailbox Response -> MsgHandler st Text
inpHandler reqBox respBox state txt = do
    send reqBox $
            Query respBox (CP.parseQuery (Namespace "") txt)
    return $ Just state

ctlHandler :: (StdBoxes Text) -> (StdBoxes Request) -> MsgHandler st ControlMsg
ctlHandler outBoxes reqBoxes _ msg = do
    send (controlBox outBoxes) msg
    send (controlBox reqBoxes) msg
    return Nothing

responseHandler :: Mailbox Text -> MsgHandler st Response
responseHandler outbox state (Response triples) = do
    send outbox $ unlines (map CS.showTriple triples)
    return $ Just state

