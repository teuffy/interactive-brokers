{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import           Control.Concurrent        (threadDelay)
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default
import           Data.Time
import           System.IO                 (stdout)
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Logger

import           API.IB 

-- -----------------------------------------------------------------------------
-- Reference data

conES :: IBContract
conES = future "ES" "ESH5" (Just $ fromGregorian 2015 03 20) GLOBEX "USD"

conES' :: IBContract
conES' = future "ES" "" Nothing GLOBEX "USD" 

-- -----------------------------------------------------------------------------
-- Requests

requests :: IB ()
requests = do
  
  display "Starting"
  displayStatus
  connect
  reqConData
  reqMktData
  recvP 50
  displayStatus
  disconnect
  delay 5
  connect
  reqMktData
  recvP 30
  delay 10
  stop
  displayStatus
  display "Finished"
  
  where
  
  reqConData = requestContractData conES'
  reqMktData = requestMarketData conES [] False
  displayStatus = status >>= display . show
  recvP n = replicateM_ n $ recv >>= display . show
  delay t = liftIO $ threadDelay (t*1000000)
  display = liftIO . putStrLn
  
-- -----------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  handler <- streamHandler stdout DEBUG >>= \h -> return $
   setFormatter h $ simpleLogFormatter "$time $loggername $prio: $msg"
  updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [handler])
  finally (runIB def requests) (threadDelay 1000000)
