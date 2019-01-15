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
    Request (..), Response (..), ResponseChan,
    setupBackend)
import Fco.Backend.Types (dbSettings, dbName, envDB, environment)
import Fco.Core.Config (setupConfigDef)
import Fco.Core.Console (setupConsole)
import Fco.Core.Messaging (
    CtlChan, CtlMsg (DoQuit, InfoMsg), 
    NotifChan, Notification (RequestQuit),
    runMainProcess)
import qualified Fco.Core.Parse as CP
import qualified Fco.Core.Show as CS
import Fco.Core.Types (Namespace (..))


-- message handlers

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


-- application

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
