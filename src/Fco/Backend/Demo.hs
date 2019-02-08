{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
--
--

module Fco.Backend.Demo (run) where

import BasicPrelude

import Control.Distributed.Process (
    Process, ProcessId, ReceivePort, SendPort,
    getSelfPid, matchChan, newChan, 
    receiveChanTimeout, receiveWait, sendChan)
import Control.Monad.Extra (whileM)

import Fco.Backend (setupEnv)
import Fco.Backend.Service (
    BackendRequest (..), BackendRespChannel, BackendResponse (..),
    BackendService, 
    startBackendSvc,
    Request (..), Response (..), ResponseChan,
    setupBackend)
import Fco.Backend.Types (dbSettings, dbName, envDB, environment)
import Fco.Core.Config (
    startConfigSvcDefault,
    setupConfigDef)
import Fco.Core.Console (setupConsole)
import Fco.Core.Messaging (
    CtlChan, CtlMsg (DoQuit, InfoMsg), 
    NotifChan, Notification (RequestQuit),
    runMainProcess)
import qualified Fco.Core.Parse as CP
import qualified Fco.Core.Show as CS
import Fco.Core.Types (Namespace (..))

import qualified Fco.Core.Service as Svc
import Fco.Core.Service (Channel,
    defaultCtlHandler, defaultListener, dummyHandler, startService)


-- new implementation, using Fco.Core.ServiceId

inpHandler :: BackendService -> BackendRespChannel -> Text -> IO Bool
inpHandler backend respChan txt = do
    Svc.send backend $
            Svc.Message (BackendQuery respChan (CP.parseQuery (Namespace "") txt))
    return True

responseHandler :: Svc.Service Text -> BackendResponse -> IO Bool
responseHandler conout (BackendResponse triples) = do
    Svc.send conout $ Svc.Message (unlines (map CS.showTriple triples))
    return True

runBackend :: IO ()
runBackend = do
    configSvc <- startConfigSvcDefault
    let db = dbSettings { dbName = "fco_test" }
    env <- setupEnv $ environment { envDB = db }
    backendSvc <- startBackendSvc env
    -- TODO: start console input and output services
    -- TODO: create channels for receiving messages from console and backend
    -- TODO: loop: receive and handle messages
    return ()


-- legacy stuff, using distributed-process

handleNotif :: Notification -> Process Bool
handleNotif (RequestQuit service) = return False
handleNotif _ = return True

handleConMsg :: SendPort Request -> SendPort Response -> Text -> Process Bool
handleConMsg reqSend respSend txt = 
    sendChan reqSend (Query respSend (CP.parseQuery (Namespace "") txt))
    >> return True

handleBeResp :: SendPort Text -> Response -> Process Bool
handleBeResp port (Response svcId triples) = 
    sendChan port (unlines (map CS.showTriple triples))
    >> return True


run :: IO ()
run = 
  runMainProcess $ do
    (self, notifRecv) <- newChan :: Process NotifChan
    (cfgReqSend, cfgId) <- setupConfigDef self
    (conWSend, conRRecv, conWId, conRId) <- setupConsole self --cfgReqSend
    let db = dbSettings { dbName = "fco_test" }
    env <- liftIO $ setupEnv $ environment { envDB = db }
    (beReqSend, beId) <- setupBackend self env
    (beRespSend, beRespRecv) <- newChan :: Process ResponseChan
    whileM $
      receiveWait [
          matchChan notifRecv $ handleNotif,
          matchChan conRRecv $ handleConMsg beReqSend beRespSend,
          matchChan beRespRecv $ handleBeResp conWSend
      ]
    sendChan cfgId DoQuit
    sendChan conWId DoQuit
    sendChan beId DoQuit
    receiveChanTimeout 1000000 notifRecv >>= print
    receiveChanTimeout 1000000 notifRecv >>= print
    receiveChanTimeout 1000000 notifRecv >>= print
