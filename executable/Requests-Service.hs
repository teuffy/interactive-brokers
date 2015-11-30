{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Applicative       ((<$>))
import           Control.Category          ((>>>))
import           Control.Concurrent        (threadDelay)
import           Control.Exception
import           Control.Lens
import           Data.Default
import           Data.Time
import           Data.Time.Zones
import           Data.Typeable
import           MVC
import           MVC.Prelude
import           MVC.Event                 hiding (handleEvent)
import           MVC.EventHandler
import           MVC.Service
import           System.IO                 (stdout)
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Logger

import           API.IB

-- -----------------------------------------------------------------------------
-- Reference data

refDate :: LocalTime 
refDate = LocalTime (fromGregorian 2015 02 06) (TimeOfDay 0 0 (toEnum 0))

conES :: IBContract
conES = future "ES" "ESZ5" (Just $ fromGregorian 2015 12 18) GLOBEX "USD" 

-- -----------------------------------------------------------------------------
-- Test strategy

data IBEventHandler a = IBEventHandler
  { _ibDone :: Bool
  , _ibClientId :: Int
  , _ibServerVersion :: Maybe Int
  , _ibServerTimeZone :: Maybe TZ
  , _ibServiceStatus :: ServiceStatus
  , _ibManagedAccounts :: [String]
  , _ibNextRequestId :: Maybe Int
  , _ibNextOrderId :: Maybe Int
  }

makeLenses ''IBEventHandler

newIBEventHandler :: EventHandler SomeEvent SomeEvent s
newIBEventHandler = mkEventHandler $ SomeEventHandler 0 IBEventHandlerAPI Just id IBEventHandler
  { _ibDone = False
  , _ibClientId = 0
  , _ibServerVersion = Nothing 
  , _ibServerTimeZone = Nothing
  , _ibServiceStatus = ServicePending
  , _ibManagedAccounts = []
  , _ibNextRequestId = Nothing
  , _ibNextOrderId = Nothing
  }

instance HandlesEvent (IBEventHandler s) where
  type AppState (IBEventHandler a) = a
  data AppStateAPI (IBEventHandler a) = IBEventHandlerAPI
  type EventIn (IBEventHandler a) = SomeEvent
  type EventOut (IBEventHandler a) = SomeEvent
  handleEvent s@IBEventHandler{} e
    | Just (IBServiceStatus x) <- fromEvent e = processServiceStatus s x
    | Just (IBResponse x@Connection{}) <- fromEvent e = processConnection s x
    | Just (IBResponse x@ManagedAccounts{}) <- fromEvent e = processManagedAccounts s x
    | Just (IBResponse x@NextValidId{}) <- fromEvent e = processNextValidId s x
    | otherwise = noEvents

type IBResult s = HandleEventResult SomeEvent SomeEvent (IBEventHandler s)

processServiceStatus :: IBEventHandler s -> ServiceStatus -> IBResult s
processServiceStatus svc@IBEventHandler{..} sts = do
  putEventHandler $ svc & (ibServiceStatus .~ sts)
  noEvents

processConnection :: IBEventHandler a -> IBResponse -> IBResult s
processConnection svc (Connection IBConnection{..}) = do
  putEventHandler $ svc 
    & (ibServerVersion .~ Just _connServerVersion) 
    & (ibServerTimeZone .~ _connServerTimeZone)
  noEvents
processConnection _ _ = noEvents

processManagedAccounts :: IBEventHandler a -> IBResponse -> IBResult s
processManagedAccounts svc@IBEventHandler{..} (ManagedAccounts acs) = do
  putEventHandler $ svc & (ibManagedAccounts .~ acs)
  noEvents
processManagedAccounts _ _ = noEvents

processNextValidId :: IBEventHandler a -> IBResponse -> IBResult s
processNextValidId svc@IBEventHandler{..} (NextValidId nid) = do
  let svc' = svc & (ibNextOrderId .~ Just nid)
  putEventHandler svc'
  if _ibDone
    then noEvents
    else processRequests svc'
processNextValidId _ _ = noEvents

processRequests :: IBEventHandler a -> IBResult s
processRequests svc@IBEventHandler{..} = do
  putEventHandler (svc & (ibDone .~ True))
  maybe noEvents return $ do
    sv <- _ibServerVersion
    oid <- _ibNextOrderId
    return
     [ 
       rq   RequestCurrentTime
    -- , rq $ RequestContractData sv 1 conES
    -- , rq $ RequestMarketData sv 2 conES [] False
    -- , rq $ RequestRealTimeBars sv 3 conES 5 BarBasisTrades False
    -- , rq $ RequestHistoricalData sv 4 conES refDate (IBDuration 1 D) 3600 BarBasisTrades False IBFDDateTime
    -- , rq $ RequestIds 3
    -- , rq $ PlaceOrder sv oid conES (marketOrder oid _ibClientId Buy 1)
     --, rq $ PlaceOrder sv boid conES (marketOrder oid _ibClientId Sell 1)
     --, rq $ CancelOrder oid
    -- , rq   RequestOpenOrders
    -- , rq   RequestAllOpenOrders
    -- , rq $ RequestAutoOpenOrders False
    -- , rq   RequestManagedAccounts
    -- , rq $ RequestAccountData sv True (head _ibManagedAccounts)
    -- , rq $ RequestAccountSummary sv 5 All [NetLiquidation]
     , rq   RequestPositions
    -- , rq   RequestManagedAccounts
    -- , rq $ CancelAccountSummary sv 5
    -- , rq $ RequestExecutions 6 newIBExecutionFilter
    -- , rq $ RequestMarketDataType RealTime
     , rq $ CancelPositions sv
     --, req $ RequestGlobalCancel sv
     ]
  where
  rq = release' . IBRequest

-- -----------------------------------------------------------------------------
-- Command handler

data CommandHandler s = CommandHandler

instance HandlesEvent (CommandHandler s) where
  type AppState (CommandHandler s) = s
  type EventIn (CommandHandler s) = SomeEvent
  type EventOut (CommandHandler s) = SomeEvent
  data AppStateAPI (CommandHandler s) = CommandHandlerAPI
  handleEvent _ e 
    | Just ("q" :: String) <- fromEvent e = return [release' Done]
    | otherwise = noEvents 

newCommandHandler :: (a -> Maybe SomeEvent) -> (EitherSomeEvent -> Either a b) -> EventHandler a b s
newCommandHandler ein eout = EventHandler [SomeEventHandler 0 CommandHandlerAPI ein eout CommandHandler]

newCommandHandler' :: EventHandler SomeEvent SomeEvent s
newCommandHandler' = newCommandHandler Just id

-- -----------------------------------------------------------------------------
-- Exit handler

data Done = Done deriving (Show,Typeable)

instance Event Done 

untilDone :: Model () SomeEvent SomeEvent
untilDone = asPipe go
  where
  go = await >>= checkDone
  checkDone e
    | Just Done <- fromEvent e = return ()
    | otherwise = yield e >> go

-- -----------------------------------------------------------------------------
-- Event handlers

eventHandler :: EventHandler SomeEvent SomeEvent s 
eventHandler = initialiseEventHandler $ mconcat
  [ newLogEventHandler (Just . show) toEitherSomeEvent
  , newIBEventHandler 
  , newCommandHandler'
  ]
 
-- -----------------------------------------------------------------------------
-- Model

model :: Model () SomeEvent SomeEvent
model = asPipe (runRecursiveEventHandler eventHandler) >>> untilDone

-- -----------------------------------------------------------------------------
-- Services

services :: Managed (View SomeEvent, Controller SomeEvent)
services = do
  (ibV,ibC) <- toManagedMVC $ processesEvent $ toManagedService $ ibService def
  let stdOutV = contramap show stdoutLines
  stdInC <- stdinLines
  return (ibV <> stdOutV,(SomeEvent <$> stdInC) <> ibC) 

-- -----------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  handler <- streamHandler stdout DEBUG >>= \h -> return $
   setFormatter h $ simpleLogFormatter "$time $loggername $prio: $msg"
  updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [handler])
  finally (runMVC () model services) (threadDelay 1000000)
