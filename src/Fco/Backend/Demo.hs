{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Backend.Demo (run) where

import BasicPrelude

import Control.Monad.Extra (whileM)

import Control.Concurrent.Actor (
    Behaviour (..), ControlMsg (..), Mailbox, MsgHandler, StdBoxes (..),
    defActor, mailbox, send, 
    spawnActor, spawnStdActor, stdBoxes)
import Control.Concurrent.Actor.Config (spawnConfigDef)
import Control.Concurrent.Actor.Console (conInLoop, conOutHandler)
import Fco.Backend (setupEnv)
import Fco.Backend.Actor (Request (..), Response (..), spawnBackend)
import Fco.Backend.Types (dbSettings, dbName, envDB, environment)
import qualified Fco.Core.Parse as CP
import qualified Fco.Core.Show as CS
import Fco.Core.Types (Namespace (..))


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


run :: IO ()
run = do
    self <- stdBoxes
    respBox <- mailbox
    confBoxes <- spawnConfigDef
    let db = dbSettings { dbName = "fco_test" }
    env <- setupEnv $ environment { envDB = db }
    reqBoxes <- spawnBackend env --confbox
    spawnActor (conInLoop self) [] ()
    outBoxes <- spawnStdActor conOutHandler ()
    defActor [
        Behv (controlBox self) (ctlHandler outBoxes reqBoxes),
        Behv (messageBox self) (inpHandler (messageBox reqBoxes) respBox),
        Behv respBox (responseHandler (messageBox outBoxes))
      ] ()
