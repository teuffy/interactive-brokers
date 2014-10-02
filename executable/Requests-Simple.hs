{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import           Control.Concurrent        (threadDelay)
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default
import           Data.Time
import           MVC.Service
import           System.IO                 (stdout)
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Logger

import           API.IB 

-- -----------------------------------------------------------------------------
-- Reference data

conESZ4 :: IBContract
conESZ4 = future "ES" "ESZ4" (fromGregorian 2014 12 19) GLOBEX "USD" 

-- -----------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  handler <- streamHandler stdout DEBUG >>= \h -> return $
   setFormatter h $ simpleLogFormatter "$time $loggername $prio: $msg"
  updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [handler])
  finally (runIB def requests) (threadDelay 1000000)

requests :: IB ()
requests = do
  
  connect
  reqMktData
  recvP 15
  disconnect
  delay 5
  reqStatus
  connect
  reqMktData
  recvP 10
  reqStatus
  recvP 10
  stop
  io "Finished"
  
  where
  
  reqMktData = requestMarketData conESZ4 [] False
  send' = void . send
  reqStatus = send' (IBServiceRequest ServiceReportStatus)
  recvP n = replicateM_ n $ recv >>= io'
  delay t = liftIO $ threadDelay (t*1000000)
  io = liftIO . putStrLn
  io' = io . show
  

