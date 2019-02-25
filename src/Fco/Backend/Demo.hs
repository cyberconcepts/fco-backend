{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Backend.Demo (run) where

import BasicPrelude

import Control.Monad.Extra (whileM)

import Control.Concurrent.Actor (
    Behaviour (..), Mailbox, Message (..), MsgHandler,
    defActor, mailbox, send, spawnActor, spawnDefActor)
import Control.Concurrent.Actor.Config (spawnConfigDef)
import Control.Concurrent.Actor.Console (conIn, conOutHandler)
import Fco.Backend (setupEnv)
import Fco.Backend.Actor (Request (..), Response (..), spawnBackend)
import Fco.Backend.Types (dbSettings, dbName, envDB, environment)
import qualified Fco.Core.Parse as CP
import qualified Fco.Core.Show as CS
import Fco.Core.Types (Namespace (..))


inpHandler :: Mailbox Request -> Mailbox Response -> MsgHandler st Text
inpHandler reqbox respbox state (Message txt) = do
    send reqbox $
            Message (Query respbox (CP.parseQuery (Namespace "") txt))
    return $ Just state
inpHandler reqbox _ _ QuitMsg = 
    (send reqbox QuitMsg) >> return Nothing

responseHandler :: Mailbox Text -> MsgHandler st Response
responseHandler outbox state (Message (Response triples)) = do
    send outbox $ Message (unlines (map CS.showTriple triples))
    return $ Just state


run :: IO ()
run = do
    inbox <- mailbox
    respbox <- mailbox
    confbox <- spawnConfigDef
    let db = dbSettings { dbName = "fco_test" }
    env <- setupEnv $ environment { envDB = db }
    reqbox <- spawnBackend env --confbox
    spawnActor (conIn inbox) [] ()
    outbox <- spawnDefActor conOutHandler ()
    defActor [
          Behaviour inbox (inpHandler reqbox respbox),
          Behaviour respbox (responseHandler outbox)]
        ()

