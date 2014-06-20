{-# LANGUAGE DeriveDataTypeable,ExistentialQuantification,GADTs,NoMonomorphismRestriction,OverloadedStrings,RankNTypes,RecordWildCards,ScopedTypeVariables,StandaloneDeriving,TemplateHaskell,TypeFamilies #-}

module Main where

import           Control.Applicative   ((<$>))
import           Control.Category      ((>>>))
import           Control.Lens
import qualified Data.ByteString.Char8 as BC
import           Data.Default
import           Data.Time
import           Data.Time.Zones
import           Data.Typeable
import           MVC
import           MVC.Prelude
import           MVC.Event             hiding (handleEvent)
import           MVC.EventHandler
import           MVC.Service

import           API.IB

-- -----------------------------------------------------------------------------
-- Reference data

refDate :: LocalTime 
refDate = LocalTime (fromGregorian 2014 06 18) (TimeOfDay 0 0 (toEnum 0))


conESU4 :: IBContract
conESU4 = newIBContract 
  { _conSymbol = "ES"
  , _conSecType = IBFuture
  , _conExpiry = "20140919"
  , _conExchange = "GLOBEX"
  , _conCurrency = "USD"
  , _conLocalSymbol = "ESU4"
  , _conPrimaryExch = "GLOBEX" 
  }

-- -----------------------------------------------------------------------------
-- Order builder

orderMkt :: Int -> Int -> IBOrderAction -> Int -> IBOrder
orderMkt oid cid oa q = newIBOrder
  { _orderId = BC.pack $ show oid
  , _orderClientId = cid
  , _orderAction = oa
  , _orderTotalQuantity = q
  , _orderType = Market
  , _orderTransmit = True
  }

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
processServiceStatus svc@IBEventHandler{..} status = do
  putEventHandler $ svc & (ibServiceStatus .~ status)
  noEvents

processConnection :: IBEventHandler a -> IBResponse -> IBResult s
processConnection svc Connection{..} = do
  putEventHandler $ svc 
    & (ibServerVersion .~ Just _connServerVersion) 
    & (ibServerTimeZone .~ _connServerTimeZone)
  noEvents
processConnection _ _ = noEvents

processManagedAccounts :: IBEventHandler a -> IBResponse -> IBResult s
processManagedAccounts svc@IBEventHandler{..} (ManagedAccounts accounts) = do
  putEventHandler $ svc & (ibManagedAccounts .~ accounts)
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
    let boid = BC.pack $ show oid
    return
     [ 
       release' $ IBRequest $ RequestCurrentTime sv
     , release' $ IBRequest $ RequestContractData sv 1 conESU4
     , release' $ IBRequest $ RequestMarketData sv 2 conESU4 [] False
     , release' $ IBRequest $ RequestRealTimeBars sv 3 conESU4 5 BarBasisTrades False
     , release' $ IBRequest $ RequestHistoricalData sv 4 conESU4 refDate (IBDuration 1 D) 3600 BarBasisTrades False IBFDDateTime
     , release' $ IBRequest $ RequestIds sv 3
     --, release' $ IBRequest $ PlaceOrder sv boid conESU4 (orderMkt oid _ibClientId Buy 1)
     , release' $ IBRequest $ RequestOpenOrders sv
     , release' $ IBRequest $ RequestAllOpenOrders sv
     , release' $ IBRequest $ RequestAutoOpenOrders sv False
     , release' $ IBRequest $ RequestManagedAccounts sv
     , release' $ IBRequest $ RequestAccountData sv True (head _ibManagedAccounts)
     , release' $ IBRequest $ RequestAccountSummary sv 5 All [NetLiquidation]
     , release' $ IBRequest $ RequestPositions sv
     , release' $ IBRequest $ RequestIds sv 3
     , release' $ IBRequest $ RequestManagedAccounts sv
     , release' $ IBRequest $ CancelAccountSummary sv 5
     , release' $ IBRequest $ RequestExecutions sv 6 newIBExecutionFilter
     , release' $ IBRequest $ RequestMarketDataType sv RealTime
     , release' $ IBRequest $ CancelPositions sv
     --, release' $ IBRequest $ RequestGlobalCancel sv
     ]

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
  (ibV,ibC) <- toManagedMVC $ processesEvent $ toManagedService $ ibService $ def & cfgDebug .~ True
  let stdOutV = contramap show stdoutLines
  stdInC <- stdinLines
  return (ibV <> stdOutV,(SomeEvent <$> stdInC) <> ibC) 

-- -----------------------------------------------------------------------------
-- Main

main :: IO ()
main = runMVC () model services
