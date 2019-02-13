{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Backend.Demo (run) where

import BasicPrelude

import Control.Monad.Extra (whileM)

import Fco.Backend (setupEnv)
import qualified Fco.Backend.Service as BE
import Fco.Backend.Service (BackendService, startBackendSvc)
import Fco.Backend.Types (dbSettings, dbName, envDB, environment)
import Fco.Core.Config (startConfigSvcDefault)
import Fco.Core.Console (conIn, conOutHandler)
import qualified Fco.Core.Parse as CP
import Fco.Core.Service (
    HandledChannel (..), Message (..), MsgHandler, Service (..),
    defaultListener, dummyHandler, multiListener,
    newChan, send, startService)
import qualified Fco.Core.Show as CS
import Fco.Core.Types (Namespace (..))


inpHandler :: BackendService -> BE.RespChannel -> MsgHandler st Text
inpHandler backend respChan state (Message txt) = do
    send backend $
            Message (BE.Query respChan (CP.parseQuery (Namespace "") txt))
    return $ Just state
inpHandler backend respChan _ QuitMsg = 
    (send backend QuitMsg) >> return Nothing

responseHandler :: Service Text -> MsgHandler st BE.Response
responseHandler out state (Message (BE.Response triples)) = do
    send out $ Message (unlines (map CS.showTriple triples))
    return $ Just state


run :: IO ()
run = do
    conRecvChan <- newChan
    backendRespChan <- newChan
    configSvc <- startConfigSvcDefault
    let db = dbSettings { dbName = "fco_test" }
    env <- setupEnv $ environment { envDB = db }
    backendSvc <- startBackendSvc env --configSvc
    conInSvc <- startService (conIn conRecvChan) dummyHandler ()
    conOutSvc <- startService defaultListener conOutHandler ()
    multiListener [
          HandledChannel conRecvChan (inpHandler backendSvc backendRespChan),
          HandledChannel backendRespChan (responseHandler conOutSvc)]
        ()

