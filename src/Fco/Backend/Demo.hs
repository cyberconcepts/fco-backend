{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Backend.Demo (run) where

import BasicPrelude

import Control.Monad.Extra (whileM)

import Control.Concurrent.Actor (
    Behaviour (..), Channel, Message (..), MsgHandler,
    defActor, newChan, send, spawnActor, spawnDefActor)
import Control.Concurrent.Actor.Config (spawnConfigDef)
import Control.Concurrent.Actor.Console (conIn, conOutHandler)
import Fco.Backend (setupEnv)
import Fco.Backend.Actor (Request (..), Response (..), spawnBackend)
import Fco.Backend.Types (dbSettings, dbName, envDB, environment)
import qualified Fco.Core.Parse as CP
import qualified Fco.Core.Show as CS
import Fco.Core.Types (Namespace (..))


inpHandler :: Channel Request -> Channel Response -> MsgHandler st Text
inpHandler reqChan respChan state (Message txt) = do
    send reqChan $
            Message (Query respChan (CP.parseQuery (Namespace "") txt))
    return $ Just state
inpHandler reqChan respChan _ QuitMsg = 
    (send reqChan QuitMsg) >> return Nothing

responseHandler :: Channel Text -> MsgHandler st Response
responseHandler out state (Message (Response triples)) = do
    send out $ Message (unlines (map CS.showTriple triples))
    return $ Just state


run :: IO ()
run = do
    conRecvChan <- newChan
    backendRespChan <- newChan
    configChan <- spawnConfigDef
    let db = dbSettings { dbName = "fco_test" }
    env <- setupEnv $ environment { envDB = db }
    backendReqChan <- spawnBackend env --configChan
    spawnActor (conIn conRecvChan) [] ()
    conOutChan <- spawnDefActor conOutHandler ()
    defActor [
          Behaviour conRecvChan (inpHandler backendReqChan backendRespChan),
          Behaviour backendRespChan (responseHandler conOutChan)]
        ()

