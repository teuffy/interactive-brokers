{-# LANGUAGE DeriveDataTypeable,ExistentialQuantification,GADTs,NoMonomorphismRestriction,OverloadedStrings,RankNTypes,RecordWildCards,ScopedTypeVariables,StandaloneDeriving,TemplateHaskell,TypeFamilies #-}

module Main where

import           Control.Lens
import qualified Data.ByteString.Char8 as BC
import           Data.Default
import           Data.Time
import           Data.Typeable
import           MVC
import           MVC.Prelude
import           MVC.Event             hiding (handleEvent)
import           MVC.Model
import           MVC.Service

import           API.IB

-- -----------------------------------------------------------------------------
-- Reference data

refDate :: UTCTime 
refDate = UTCTime (fromGregorian 2014 04 16) 0

conESM4 :: IBContract
conESM4 = newIBContract 
  { _conSymbol = "ES"
  , _conSecType = IBFuture
  , _conExpiry = "20140620"
  , _conExchange = "GLOBEX"
  , _conCurrency = "USD"
  , _conLocalSymbol = "ESM4"
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
-- Test strategy app service

data IBAppService a = IBAppService
  { _ibServiceId :: Int
  , _ibDone :: Bool
  , _ibClientId :: Int
  , _ibServerVersion :: Maybe Int
  , _ibServerTimeZone :: Maybe TimeZone
  , _ibServiceStatus :: ServiceStatus
  , _ibManagedAccounts :: [String]
  , _ibNextRequestId :: Maybe Int
  , _ibNextOrderId :: Maybe Int
  }

makeLenses ''IBAppService

newIBAppService :: SomeAppService a
newIBAppService = SomeAppService 0 IBAppServiceAPI IBAppService
  { _ibServiceId = 0
  , _ibDone = False
  , _ibClientId = 0
  , _ibServerVersion = Nothing 
  , _ibServerTimeZone = Nothing
  , _ibServiceStatus = ServicePending
  , _ibManagedAccounts = []
  , _ibNextRequestId = Nothing
  , _ibNextOrderId = Nothing
  }

instance AppService (IBAppService a) where
  type AppState (IBAppService a) = a
  data AppStateAPI (IBAppService a) = IBAppServiceAPI
  processEvent s@IBAppService{} e
    | Just (IBServiceStatus x) <- fromEvent e = processServiceStatus s x
    | Just (IBResponse x@Connection{}) <- fromEvent e = processConnection s x
    | Just (IBResponse x@ManagedAccounts{}) <- fromEvent e = processManagedAccounts s x
    | Just (IBResponse x@NextValidId{}) <- fromEvent e = processNextValidId s x
    | otherwise = noEvents

processServiceStatus :: IBAppService a -> ServiceStatus -> AppServiceModelEvents (IBAppService a)
processServiceStatus svc@IBAppService{..} status = do
  putAppService $ svc & (ibServiceStatus .~ status)
  noEvents

processConnection :: IBAppService a -> IBResponse -> AppServiceModelEvents (IBAppService a)
processConnection svc Connection{..} = do
  putAppService $ svc 
    & (ibServerVersion .~ Just _connServerVersion) 
    & (ibServerTimeZone .~ Just _connServerTimeZone)
  noEvents
processConnection _ _ = noEvents

processManagedAccounts :: IBAppService a -> IBResponse -> AppServiceModelEvents (IBAppService a)
processManagedAccounts svc@IBAppService{..} (ManagedAccounts accounts) = do
  putAppService $ svc & (ibManagedAccounts .~ accounts)
  noEvents
processManagedAccounts _ _ = noEvents

processNextValidId :: IBAppService a -> IBResponse -> AppServiceModelEvents (IBAppService a)
processNextValidId svc@IBAppService{..} (NextValidId nid) = do
  let svc' = svc & (ibNextOrderId .~ Just nid)
  putAppService svc'
  if _ibDone
    then noEvents
    else processRequests svc'
processNextValidId _ _ = noEvents

processRequests :: IBAppService a -> AppServiceModelEvents (IBAppService a)
processRequests svc@IBAppService{..} = do
  putAppService (svc & (ibDone .~ True))
  maybe noEvents return $ do
    sv <- _ibServerVersion
    oid <- _ibNextOrderId
    let boid = BC.pack $ show oid
    return
     [ release $ IBRequest $ RequestCurrentTime sv
     , release $ IBRequest $ RequestContractData sv 1 conESM4
     , release $ IBRequest $ RequestMarketData sv 2 conESM4 [] False
     , release $ IBRequest $ RequestRealTimeBars sv 3 conESM4 5 BarBasisTrades False
     , release $ IBRequest $ RequestHistoricalData sv 4 conESM4 refDate (IBDuration 1 D) 3600 BarBasisTrades False IBFDDateTime
     , release $ IBRequest $ RequestIds sv 3
     , release $ IBRequest $ PlaceOrder sv boid conESM4 (orderMkt oid _ibClientId Buy 1)
     , release $ IBRequest $ RequestOpenOrders sv
     , release $ IBRequest $ RequestAllOpenOrders sv
     , release $ IBRequest $ RequestAutoOpenOrders sv False
     , release $ IBRequest $ RequestManagedAccounts sv
     , release $ IBRequest $ RequestAccountData sv True (head _ibManagedAccounts)
     , release $ IBRequest $ RequestAccountSummary sv 5 All [NetLiquidation]
     , release $ IBRequest $ RequestPositions sv
     , release $ IBRequest $ RequestIds sv 3
     , release $ IBRequest $ RequestManagedAccounts sv
     , release $ IBRequest $ CancelAccountSummary sv 5
     , release $ IBRequest $ RequestExecutions sv 6 newIBExecutionFilter
     , release $ IBRequest $ RequestMarketDataType sv RealTime
     , release $ IBRequest $ CancelPositions sv
     , release $ IBRequest $ RequestGlobalCancel sv
     ]

-- -----------------------------------------------------------------------------
-- Log app service

data Msg = Msg String deriving (Typeable,Show)

instance Event Msg

data LogAppService a = LogAppService

instance AppService (LogAppService a) where
  type AppState (LogAppService a) = a
  data AppStateAPI (LogAppService a) = LogAppServiceAPI
  processEvent _ e = return [release . Msg $ show e]

newLogAppService :: SomeAppService a
newLogAppService = SomeAppService 0 LogAppServiceAPI LogAppService

-- -----------------------------------------------------------------------------
-- App services

appServices :: [SomeAppService a]
appServices = initialiseAppServices
  [ newIBAppService
  , newLogAppService
  ]
 
-- -----------------------------------------------------------------------------
-- Services

services :: Managed (View SomeEvent, Controller SomeEvent)
services = do
  (ibV,ibC) <- toManagedMVC $ processesEvent $ toManagedService $ ibService def
  let stdOutV = contramap show stdoutLines
  return (ibV <> stdOutV,ibC) 

-- -----------------------------------------------------------------------------
-- Main

main :: IO ()
main = runMVC () (asPipe $ runAppModel appServices handleEvent) services
