{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Backend.Demo (run) where

import BasicPrelude

import Control.Monad.Extra (whileM)

import Fco.Backend (setupEnv)
import Fco.Backend.Service (
    BackendRequest (..), BackendRespChannel, BackendResponse (..),
    BackendService, 
    startBackendSvc)
import Fco.Backend.Types (dbSettings, dbName, envDB, environment)
import Fco.Core.Config (startConfigSvcDefault)
import qualified Fco.Core.Parse as CP
import Fco.Core.Service (
    Channel, HandledChannel (..), Message (..), Service (..),
    conIn, conOutHandler, defaultCtlHandler, defaultListener, dummyHandler, 
    newChan, receiveChanAny, send, startService)
import qualified Fco.Core.Show as CS
import Fco.Core.Types (Namespace (..))


inpHandler :: BackendService -> BackendRespChannel -> Message Text -> IO Bool
inpHandler backend respChan (Message txt) = do
    send backend $
            Message (BackendQuery respChan (CP.parseQuery (Namespace "") txt))
    return True
inpHandler backend respChan (QuitMsg) = 
    (send backend QuitMsg) >> return False

responseHandler :: Service Text -> Message BackendResponse -> IO Bool
responseHandler conout (Message (BackendResponse triples)) = do
    send conout $ Message (unlines (map CS.showTriple triples))
    return True


run :: IO ()
run = do
    conRecvChan <- newChan
    backendRespChan <- newChan
    configSvc <- startConfigSvcDefault
    let db = dbSettings { dbName = "fco_test" }
    env <- setupEnv $ environment { envDB = db }
    backendSvc <- startBackendSvc env
    conInSvc <- startService (conIn conRecvChan) dummyHandler ()
    conOutSvc <- startService defaultListener conOutHandler ()
    whileM $ receiveChanAny [
        HandledChannel conRecvChan (inpHandler backendSvc backendRespChan),
        HandledChannel backendRespChan (responseHandler conOutSvc)]

