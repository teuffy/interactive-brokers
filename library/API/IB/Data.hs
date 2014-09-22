{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

module API.IB.Data where

import           Control.Applicative
import           Control.Arrow                       (second)
import           Control.Lens                        (makeLenses)
import           Control.Monad
import           Currency
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                     (ByteString)
import qualified Data.IntMap                         as IntMap (lookup)
import           Data.List.Split                     (splitOn)
import           Data.Map                            (Map)
import qualified Data.Map                            as Map (empty, fromList, lookup)
import           Data.Maybe
import           Data.Time
import           Data.Time.Zones

import           API.IB.Constant
import           API.IB.Enum
import           API.IB.Parse
import           API.IB.Util

-- -----------------------------------------------------------------------------
-- Types

type IBTagValues = Map String String 

parseTagValues :: Parser IBTagValues
parseTagValues = do
  count' <- parseIntField
  Map.fromList <$> replicateM count' ((,) <$> parseStringField <*> parseStringField)

-- -----------------------------------------------------------------------------
-- Request

data IBRequest =
    RequestMarketData 
    { _reqServerVersion :: Int
    , _reqTickerId :: Int 
    , _reqContract :: IBContract
    , _reqTickerList :: [IBGenericTickType]
    , _reqSnapshotFlag :: Bool
    } 
  | CancelMarketData
    { _reqServerVersion :: Int
    , _reqTickerId :: Int
    }
  | PlaceOrder 
    { _reqServerVersion :: Int
    , _reqOrderId :: ByteString 
    , _reqContract :: IBContract
    , _reqOrder :: IBOrder
    }
  | CancelOrder
    { _reqServerVersion :: Int 
    , _reqOrderId :: ByteString
    }
  | RequestOpenOrders 
    { _reqServerVersion :: Int
    }
  | RequestAccountData 
    { _reqServerVersion :: Int
    , _reqSubscribe :: Bool 
    , _reqAccount :: String
    }
  | RequestExecutions 
    { _reqServerVersion :: Int
    , _reqRequestId :: Int 
    , _reqExecutionFilter :: IBExecutionFilter
    }
  | RequestIds 
    { _reqServerVersion :: Int 
    , _reqNumIds :: Int
    }
  | RequestContractData 
    { _reqServerVersion :: Int
    , _reqRequestId :: Int 
    , _reqContract :: IBContract
    }
  | RequestAutoOpenOrders 
    { _reqServerVersion :: Int
    , _reqAutoBind :: Bool
    }
  | RequestAllOpenOrders 
    { _reqServerVersion :: Int
    }
  | RequestManagedAccounts 
    { _reqServerVersion :: Int
    }
  | RequestHistoricalData 
    { _reqServerVersion :: Int
    , _reqTickerId :: Int 
    , _reqContract :: IBContract 
    , _reqEndDateTime :: LocalTime 
    , _reqDuration :: IBDuration 
    , _reqBarSize :: Int 
    , _reqBarBasis :: IBBarBasis 
    , _reqRegularTradingHours :: Bool 
    , _reqFormatDate :: IBFormatDate
    }
  | CancelHistoricalData 
    { _reqServerVersion :: Int
    , _reqTickerId :: Int
    }
  | RequestCurrentTime 
    { _reqServerVersion :: Int
    }
  | RequestRealTimeBars 
    { _reqServerVersion :: Int
    , _reqTickerId :: Int 
    , _reqContract :: IBContract 
    , _reqBarSize :: Int 
    , _reqBarBasis :: IBBarBasis 
    , _reqRegularTradingHours :: Bool
    }
  | CancelRealTimeBars 
    { _reqServerVersion :: Int 
    , _reqTickerId :: Int
    }
  | RequestGlobalCancel 
    { _reqServerVersion :: Int
    }
  | RequestMarketDataType 
    { _reqServerVersion :: Int
    , _reqMarketDataType :: IBMarketDataType
    }
  | RequestPositions 
    { _reqServerVersion :: Int
    }
  | RequestAccountSummary 
    { _reqServerVersion :: Int
    , _reqRequestId :: Int 
    , _reqGroup :: IBGroup 
    , _reqTags :: IBTags
    }
  | CancelAccountSummary 
    { _reqServerVersion :: Int 
    , _req :: Int
    }
  | CancelPositions 
    { _reqServerVersion :: Int
    }
  deriving Show

-- -----------------------------------------------------------------------------
-- Response

data IBHistoricalDataItem = IBHistoricalDataItem 
    { _hdiDate :: Day
    , _hdiTime :: TimeOfDay
    , _hdiOpen :: Double 
    , _hdiHigh :: Double 
    , _hdiLow :: Double 
    , _hdiClose :: Double 
    , _hdiVolume :: Int 
    , _hdiWAP :: Double 
    , _hdiHasGaps :: Bool 
    , _hdiBarCount :: Int
    } 
    deriving Show

data IBResponse = 
    Connection 
    { _connServerVersion :: Int
    , _connServerTime :: LocalTime
    , _connServerTimeZoneDesc :: String
    , _connServerTimeZone :: Maybe TZ
    }
  | TickPrice 
    { _tpTickerId :: Int
    , _tpTickType :: IBTickType 
    , _tpPrice :: Double
    , _tpCanAutoExecute :: Bool
    }
  | TickSize 
    { _tsTickerId :: Int
    , _tsTickType :: IBTickType 
    , _tsSize :: Int
    }
  | OrderStatus 
    { _osOrderId :: ByteString
    , _osOrderStatus :: IBOrderStatus
    , _osFilled :: Int
    , _osRemaining :: Int
    , _osAvgFillPrice :: Double 
    , _osPermId :: Int
    , _osParentId :: Int 
    , _osLastFillPrice :: Double
    , _osClientId :: Int
    , _osWhyHeld :: IBOrderWhyHeld
    } 
  | Error 
    { _errId :: Int 
    , _errCode :: Int 
    , _errDesc :: String
    }
  | OpenOrder 
    { _ooOrderId :: ByteString
    , _ooContract :: IBContract
    , _ooOrder :: IBOrder
    , _ooOrderState :: IBOrderState
    }
  | HistoricalData
    { _hdRequestId :: Int
    , _hdItems :: [IBHistoricalDataItem]
    }
  | RealTimeBar
    { _rtbReqId :: Int
    , _rtbTime :: UTCTime -- Int
    , _rtbOpen :: Double
    , _rtbHigh :: Double
    , _rtbLow :: Double
    , _rtbClose :: Double
    , _rtbVolume :: Int
    , _rtbWAP :: Double
    , _rtbCount :: Int
    } 
  | NextValidId 
    { _nvId :: Int
    } 
  | ContractData
    { _cdReqId :: Int
    , _cdContractDetails :: IBContractDetails
    }
  | ContractDataEnd 
    { _cdReqId :: Int
    }
  | ManagedAccounts
    { _maAccounts :: [String]
    }
  | TickGeneric 
    { _tgTickerId :: Int
    , _tgTickType :: IBTickType
    , _tgValue :: Double
    }
  | TickString
    { _tsTickerId :: Int
    , _tsTickType :: IBTickType
    , _tsValue :: String
    }
  | TickTime
    { _tsTickerId :: Int
    , _tsTickType :: IBTickType
    , _tsDateTime :: UTCTime
    }
  | CurrentTime
    { _ctDateTime :: UTCTime
    }
  | Position
    { _posAccount :: String
    , _posContract :: IBContract
    , _posPosition :: Int
    , _posAvgCost :: Double
    }
  | PositionEnd
  | AccountSummary 
    { _asReqId :: Int
    , _asAccount :: String
    , _asTag :: IBTag
    , _asValue :: String
    , _asCurrency :: Currency
    }
  | AccountSummaryEnd
    { _asReqId :: Int
    }
  | OpenOrderEnd
  | AccountValue
    { _avTag :: IBTag
    , _avValue :: String
    , _avCurrency :: Maybe Currency
    , _avAccount :: ByteString
    }
  | PortfolioValue
    { _pvContract :: IBContract
    , _pvPosition :: Int 
    , _pvMarketPrice :: Double
    , _pvMarketValue :: Double 
    , _pvAverageCost :: Double 
    , _pvUnrealisedPnL :: Double 
    , _pvRealisedPnL :: Double 
    , _pvAccount :: String
    }
  | AccountUpdateTime 
    { _acUpdateTime :: TimeOfDay
    }
  | ExecutionData 
    { _exReqId :: Int
    , _exContract :: IBContract 
    , _exExecution :: IBExecution
    }
  | ExecutionDataEnd
    { _exReqId :: Int
    }
  | TickSnapshotEnd 
    { _tssReqId :: Int
    }
  | MarketDataType
    { _mdtReqId :: Int
    , _mdtMarketDataType :: IBMarketDataType
    }
  | CommissionReport 
    { _crCommissionReport :: IBCommissionReport
    }
  | AccountDownloadEnd 
    { _acAccount :: String
    }
  deriving Show

type IBResponses = Map IBResponseType (Parser [IBResponse])  

responses :: IBResponses
responses = Map.fromList $
  (TickPriceT,parseTickPrice) : 
  map (second ((:[]) <$>)) [ 
  (TickSizeT,parseTickSize),
  (OrderStatusT,parseOrderStatus),
  (ErrorMessageT,parseError),
  (OpenOrderT,parseOpenOrder),
  (NextValidIdT,parseNextValidId),
  (ContractDataT,parseContractData),
  (ContractDataEndT,parseContractDataEnd),
  (ManagedAccountsT,parseManagedAccounts),
  (HistoricalDataT,parseHistoricalData),
  (TickGenericT,parseTickGeneric),
  (TickStringT,parseTickString),
  (CurrentTimeT,parseCurrentTime),
  (RealTimeBarT,parseRealTimeBar),
  (PositionT,parsePosition),
  (PositionEndT,parsePositionEnd),
  (AccountSummaryT,parseAccountSummary),
  (AccountSummaryEndT,parseAccountSummaryEnd),
  (OpenOrderEndT,parseOpenOrderEnd),
  (AccountValueT,parseAccountValue),
  (PortfolioValueT,parsePortfolioValue),
  (AccountUpdateTimeT,parseAccountUpdateTime),
  (ExecutionDataT,parseExecutionData),
  (ExecutionDataEndT,parseExecutionDataEnd),
  (TickSnapshotEndT,parseTickSnapshotEnd),
  (MarketDataTypeT,parseMarketDataType),
  (CommissionReportT,parseCommissionReport),
  (AccountDownloadEndT,parseAccountDownloadEnd)
  ]

parseIBResponses :: Parser [IBResponse]
parseIBResponses  = 
  parseTypedResponses <|> 
  ((:[]) <$> parseConnection)

parseResponseType :: Parser Int
parseResponseType = parseIntField

parseTypedResponses :: Parser [IBResponse]
parseTypedResponses = do
  i <- parseResponseType
  case IntMap.lookup i ibResponseTypesM' of
    Nothing -> fail ""
    Just t -> fromMaybe (fail "") (Map.lookup t responses)

parseConnection :: Parser IBResponse
parseConnection = do
  sv <- parseField decimal
  d <- parseDayYYYYMMDD <* skipSpace
  t <- parseTimeOfDayHHMMSS <* skipSpace
  tz <- parseStringField
  return $ Connection sv (LocalTime d t) tz Nothing

parseVersion :: Parser Int
parseVersion = parseIntField

parseTickPrice :: Parser [IBResponse]
parseTickPrice = do
  v <- parseVersion
  tid <- parseIntField 
  tt <- parseField parseIBTickType
  -- let stt = Map.lookup tt $ Map.fromList [(Bid,BidSize),(Ask,AskSize),(Last,LastSize)]
  p <- fromMaybe 0 <$> parseMaybeDoubleField 
  _ <- if v >= 2 then parseIntField else return 0
  c <- if v >= 3 then parseBoolBinaryField else return False
  return [TickPrice tid tt p c] -- (maybe [] (\stt' -> [TickSize tid stt' s]) stt) -- looks like a separate TickSize is always sent

parseTickSize :: Parser IBResponse
parseTickSize = do
  _ <- parseVersion
  TickSize <$> 
    parseIntField <*>
    parseField parseIBTickType <*>
    parseIntField 

parseOrderStatus :: Parser IBResponse
parseOrderStatus = do
  v <- parseVersion
  OrderStatus <$>
    parseByteStringField <*>
    parseField parseIBOrderStatus <*>
    parseIntField <*>
    parseIntField <*>
    (fromMaybe 0 <$> parseMaybeDoubleField) <*>
    (if v >= 2 then parseIntField else return 0) <*>
    (if v >= 3 then parseIntField else return 0) <*>
    (if v >= 4 then fromMaybe 0 <$> parseMaybeDoubleField else return 0.0) <*>
    (if v >= 5 then parseIntField else return 0) <*>
    (if v >= 6 
      then parseField parseIBOrderWhyHeld <|> (return NoReason <* parseEmptyField)
      else return NoReason)

parseError :: Parser IBResponse
parseError = do
  _ <- parseVersion
  Error <$>
    parseSignedIntField <*>
    parseIntField <*>
    parseStringField 

parseOpenOrder :: Parser IBResponse
parseOpenOrder = do
  v <- parseVersion
  orderId' <- parseByteStringField
  contract <- 
    bConId newIBContract 
      >>= bConSymbol 
      >>= bConSecType 
      >>= bConExpiry 
      >>= bConStrike 
      >>= bConRight 
      >>= when' (v>=32) bConMultiplier
      >>= bConExchange
      >>= bConCurrency 
      >>= bConLocalSymbol 
      >>= when' (v>=32) bConTradingClass
  (order,contract') <- parseIBOrder contract
  orderState <- parseIBOrderState
  return $ OpenOrder orderId' contract' order{_orderId = orderId'} orderState 

parseNextValidId :: Parser IBResponse
parseNextValidId = do
  _ <- parseVersion
  NextValidId <$> parseIntField

parseContractData :: Parser IBResponse
parseContractData = do
  _ <- parseVersion
  reqId' <- parseIntField
  contract' <- 
    bConSymbol newIBContract 
      >>= bConSecType 
      >>= bConExpiry 
      >>= bConStrike 
      >>= bConRight 
      >>= bConExchange 
      >>= bConCurrency 
      >>= bConLocalSymbol
  cdMarketName' <- parseStringField
  contract'' <- bConTradingClass contract' >>= bConId
  cdMinTick' <- parseDoubleField
  contract''' <- bConMultiplier contract''
  cdOrderTypes' <- parseStringField
  cdValidExchanges' <- parseStringField
  cdPriceMagnifier' <- parseIntField
  cdUnderConId' <- parseIntField
  cdLongName' <- parseStringField
  contract'''' <- bConPrimaryExch contract'''
  cdContractMonth' <- parseStringField
  cdIndustry' <- parseStringField
  cdCategory' <- parseStringField
  cdSubCategory' <- parseStringField
  cdTimeZoneId' <- parseStringField
  cdTradingHours' <- parseStringField
  cdLiquidHours' <- parseStringField
  cdEvRule' <- parseStringField
  cdEvMultiplier' <- parseDoubleField'
  numSecIds <- parseIntField
  secIdsList <- count numSecIds $ (,) <$> parseStringField <*> parseStringField
  let 
    contractDetails' = newIBContractDetails 
      { _cdSummary = contract''''
      , _cdMarketName = cdMarketName'
      , _cdMinTick = cdMinTick'
      , _cdPriceMagnifier = cdPriceMagnifier'
      , _cdOrderTypes = cdOrderTypes'
      , _cdValidExchanges = cdValidExchanges'
      , _cdUnderConId = cdUnderConId'
      , _cdLongName = cdLongName'
      , _cdContractMonth = cdContractMonth'
      , _cdIndustry = cdIndustry'
      , _cdCategory = cdCategory'
      , _cdSubCategory = cdSubCategory'
      , _cdTimeZoneId = cdTimeZoneId'
      , _cdTradingHours = cdTradingHours'
      , _cdLiquidHours = cdLiquidHours'
      , _cdEvRule = cdEvRule'
      , _cdEvMultiplier = cdEvMultiplier'
      , _cdSecIds = Map.fromList secIdsList
      }
  return $ ContractData reqId' contractDetails'

parseContractDataEnd :: Parser IBResponse
parseContractDataEnd = do
  _ <- parseVersion
  ContractDataEnd <$> 
    parseIntField 

parseManagedAccounts :: Parser IBResponse
parseManagedAccounts = do
  _ <- parseVersion
  (ManagedAccounts . splitOn ",") <$> parseStringField

parseHistoricalData :: Parser IBResponse
parseHistoricalData = do
  _ <- parseVersion
  reqid <- parseIntField
  _ <- parseStringField
  _ <- parseStringField
  items <- parseIntField
  HistoricalData reqid <$> count items parseHistoricalDataItem 

parseHistoricalDataItem :: Parser IBHistoricalDataItem
parseHistoricalDataItem = 
  IBHistoricalDataItem <$>
    (parseDayYYYYMMDD <* skipSpace) <*>
    (parseTimeOfDayHHMMSS <* char sepC) <*>
    parseDoubleField <*>
    parseDoubleField <*>
    parseDoubleField <*>
    parseDoubleField <*>
    parseSignedIntField <*>
    parseSignedDoubleField <*>
    parseBoolStringField <*>
    parseSignedIntField

parseTickGeneric :: Parser IBResponse
parseTickGeneric = do
  _ <- parseVersion
  TickGeneric <$> 
    parseIntField <*>
    parseField parseIBTickType <*>
    parseDoubleField

parseTickString :: Parser IBResponse
parseTickString = do
  _ <- parseVersion
  i <- parseIntField
  t <- parseField parseIBTickType
  case t of
    LastTimestamp -> TickTime i t <$> parseUTCTimeField
    _ -> TickString i t <$> parseStringField 

parseCurrentTime :: Parser IBResponse
parseCurrentTime = do
  _ <- parseVersion
  CurrentTime <$> parseUTCTimeField 

parseRealTimeBar :: Parser IBResponse
parseRealTimeBar = do
  _ <- parseVersion
  RealTimeBar <$>
    parseIntField <*>
    parseUTCTimeField <*> -- parseIntField <*>
    parseDoubleField <*>
    parseDoubleField <*>
    parseDoubleField <*>
    parseDoubleField <*>
    parseIntField <*>
    parseDoubleField <*>
    parseIntField

parsePosition :: Parser IBResponse
parsePosition = do
  v <- parseVersion
  Position <$>
    parseStringField <*>
    (bConId newIBContract 
      >>= bConSymbol 
      >>= bConSecType 
      >>= bConExpiry 
      >>= bConStrike 
      >>= bConRight 
      >>= bConMultiplier 
      >>= bConExchange 
      >>= bConCurrency 
      >>= bConLocalSymbol 
      >>= when' (v>=2) bConTradingClass) <*>
    parseSignedIntField <*>
    if v >= 3 then parseDoubleField else return 0.0

parsePositionEnd :: Parser IBResponse
parsePositionEnd = do
  _ <- parseVersion
  return PositionEnd

parseAccountSummary :: Parser IBResponse
parseAccountSummary = do
  _ <- parseVersion
  AccountSummary <$>
    parseIntField <*>
    parseStringField <*>
    parseField parseIBTag <*>
    parseStringField <*>
    parseField parseStringToEnum

parseAccountSummaryEnd :: Parser IBResponse
parseAccountSummaryEnd = do
  _ <- parseVersion
  AccountSummaryEnd <$>
    parseIntField

parseOpenOrderEnd :: Parser IBResponse
parseOpenOrderEnd = do
  _ <- parseVersion
  return OpenOrderEnd    

parseAccountDownloadEnd :: Parser IBResponse
parseAccountDownloadEnd = do
  _ <- parseVersion
  AccountDownloadEnd <$>
    parseStringField

parseAccountValue :: Parser IBResponse
parseAccountValue = do
  v <- parseVersion  
  AccountValue <$>
    parseField parseIBTag <*>
    parseStringField <*>
    parseMaybeEmptyField parseStringToEnum <*>
    if v >= 2 then parseByteStringField else return ""

parsePortfolioValue :: Parser IBResponse
parsePortfolioValue = do
  v <- parseVersion
  PortfolioValue <$>
    parseContract' v <*>
    parseSignedIntField <*>
    parseDoubleField <*>
    parseDoubleField <*>
    (if v >= 3 then parseDoubleField  else return 0.0) <*>
    (if v >= 3 then parseDoubleField  else return 0.0) <*>
    (if v >= 3 then parseDoubleField  else return 0.0) <*>
    (if v >= 4 then parseStringField else return "")
    where
    parseContract' v = 
      (if v>=6 then bConId newIBContract >>= bConSymbol else bConSymbol newIBContract)
      >>= bConSecType 
      >>= bConExpiry 
      >>= bConStrike 
      >>= bConRight 
      >>= when' (v>=7) (bConMultiplier >=> bConPrimaryExch)
      >>= bConCurrency 
      >>= bConLocalSymbol 
      >>= when' (v>=8) bConTradingClass
    
parseAccountUpdateTime :: Parser IBResponse
parseAccountUpdateTime = do
  _ <- parseVersion
  AccountUpdateTime <$>
    parseField parseTimeOfDayHHMM

parseExecutionData :: Parser IBResponse
parseExecutionData = do
  v <- parseVersion
  reqId' <- if v >= 7 then parseSignedIntField else return (-1)
  execOrderId' <- parseByteStringField
  contract' <- 
    bConId newIBContract
    >>= bConSymbol 
    >>= bConSecType 
    >>= bConExpiry 
    >>= bConStrike 
    >>= bConRight 
    >>= when' (v>=9) bConMultiplier
    >>= bConExchange 
    >>= bConCurrency 
    >>= bConLocalSymbol 
    >>= when' (v >=10) bConTradingClass
  let execution = newIBExecution
  execId' <- parseByteStringField
  --execTime' <- parseStringField
  execDay' <- parseDayYYYYMMDD <* skipSpace
  execTime' <- parseTimeOfDayHHMMSS <* char sepC
  execAcctNumber' <- parseStringField
  execExchange' <- parseStringField
  --execSide' <- parseStringField  
  execSide' <- parseField parseIBExecutionSide
  execShares' <- parseIntField
  execPrice' <- parseDoubleField
  execPermId' <- parseIntField
  execClientId' <- parseIntField
  execLiquidation' <- parseIntField
  execCumQty' <- if v >= 6 then parseIntField else return $ _execCumQty execution
  execAvgPrice' <- if v >= 6 then parseDoubleField else return $ _execAvgPrice execution
  execOrderRef' <- if v >= 8 then parseStringField else return $ _execOrderRef execution
  execEvRule' <- if v >= 9 then parseStringField else return $ _execEvRule execution
  execEvMultiplier' <- if v >= 9 then parseDoubleField' else return $ _execEvMultiplier execution
  let execution' = execution { 
      _execOrderId = execOrderId'
    , _execClientId = execClientId'
    , _execId = execId'
    , _execTime = LocalTime execDay' execTime'
    , _execAcctNumber = execAcctNumber'
    , _execExchange = execExchange'
    , _execSide = execSide'
    , _execShares = execShares'
    , _execPrice = execPrice'
    , _execPermId = execPermId'
    , _execLiquidation = execLiquidation'
    , _execCumQty = execCumQty'
    , _execAvgPrice = execAvgPrice'
    , _execOrderRef = execOrderRef'
    , _execEvRule = execEvRule'
    , _execEvMultiplier = execEvMultiplier'
    }
  return $ ExecutionData reqId' contract' execution'

parseExecutionDataEnd :: Parser IBResponse
parseExecutionDataEnd = do
  _ <- parseVersion
  ExecutionDataEnd <$>
    parseIntField

parseTickSnapshotEnd :: Parser IBResponse
parseTickSnapshotEnd = do
  _ <- parseVersion
  TickSnapshotEnd <$>
    parseIntField

parseMarketDataType :: Parser IBResponse  
parseMarketDataType = do 
  _ <- parseVersion
  MarketDataType <$>
    parseIntField <*>
    parseField parseIBMarketDataType 

parseCommissionReport :: Parser IBResponse
parseCommissionReport = do
  _ <- parseVersion
  CommissionReport <$> 
    parseIBCommissionReport

-- -----------------------------------------------------------------------------
-- Contract

data IBContract = IBContract 
  { _conId :: Int
  , _conSymbol :: String
  , _conSecType :: IBSecurityType -- String 
  , _conExpiry :: String
  , _conStrike :: Double 
  , _conRight :: String
  , _conMultiplier :: String
  , _conExchange :: String
  , _conCurrency :: String
  , _conLocalSymbol :: String
  , _conTradingClass :: String
  , _conPrimaryExch :: String  -- not SMART
  , _conIncludeExpired :: Bool  -- False for orders
  , _conSecIdType :: String  -- CUSIP;SEDOL;ISIN;RIC
  , _conSecId :: String
  , _conComboLegsDescrip :: String  -- received in open order version 14 and up for all combos
  , _conComboLegs :: [IBComboLeg]
  , _conUnderComp :: Maybe IBUnderComp  -- delta-neutral
  } deriving Show

newIBContract :: IBContract
newIBContract = IBContract 
  { _conId = 0
  , _conSymbol = ""
  , _conSecType = IBForex
  , _conExpiry = ""
  , _conStrike = 0.0
  , _conRight = ""
  , _conMultiplier = ""
  , _conExchange = ""
  , _conCurrency = ""
  , _conLocalSymbol = ""
  , _conTradingClass = ""
  , _conPrimaryExch = ""
  , _conIncludeExpired = False
  , _conSecIdType = ""
  , _conSecId = ""
  , _conComboLegsDescrip = ""
  , _conComboLegs = []
  , _conUnderComp = Nothing
  }

bConId :: IBContract -> Parser IBContract
bConId contract = parseIntField >>= \f -> return contract {_conId = f}

bConSymbol :: IBContract -> Parser IBContract
bConSymbol contract = parseStringField >>= \f -> return contract {_conSymbol = f}

bConSecType :: IBContract -> Parser IBContract
bConSecType contract = parseField parseIBSecurityType >>= \f -> return contract {_conSecType = f}

bConExpiry :: IBContract -> Parser IBContract
bConExpiry contract = parseStringField >>= \f -> return contract {_conExpiry = f}

bConStrike :: IBContract -> Parser IBContract
bConStrike contract = parseDoubleField >>= \f -> return contract {_conStrike= f}

bConRight :: IBContract -> Parser IBContract
bConRight contract = parseStringField >>= \f -> return contract {_conRight = f}

bConMultiplier :: IBContract -> Parser IBContract
bConMultiplier contract = parseStringField >>= \f -> return contract {_conMultiplier = f}

bConExchange :: IBContract -> Parser IBContract
bConExchange contract = parseStringField >>= \f -> return contract {_conExchange = f}

bConCurrency :: IBContract -> Parser IBContract
bConCurrency contract = parseStringField >>= \f -> return contract {_conCurrency = f}

bConLocalSymbol :: IBContract -> Parser IBContract
bConLocalSymbol contract = parseStringField >>= \f -> return contract {_conLocalSymbol = f}

bConTradingClass :: IBContract -> Parser IBContract
bConTradingClass contract = parseStringField >>= \f -> return contract {_conTradingClass = f}

bConPrimaryExch :: IBContract -> Parser IBContract
bConPrimaryExch contract = parseStringField >>= \f -> return contract {_conPrimaryExch = f}

parseIBContractComboLegs :: IBContract -> Parser IBContract
parseIBContractComboLegs contract = do
  conComboLegsDescrip' <- parseStringField
  conComboLegsCount <- parseIntField
  conComboLegs' <- replicateM conComboLegsCount parseIBComboLeg
  return contract 
    { _conComboLegsDescrip = conComboLegsDescrip'
    , _conComboLegs = conComboLegs'
    }

parseIBContractUnderComp :: IBContract -> Parser IBContract
parseIBContractUnderComp contract = do
  under' <- parseBoolBinaryField
  conUnderComp' <- if under' 
    then
      Just <$> parseIBUnderComp
    else
      return Nothing
  return contract{_conUnderComp = conUnderComp'}

-- -----------------------------------------------------------------------------
-- Combo leg

data IBComboLeg = IBComboLeg 
  { _comConId :: Int
  , _comRatio :: Int
  , _comAction :: String  -- BUY/SELL/SSHORT/SSHORTX
  , _comExchange :: String
  , _comOpenClose :: Int
  -- for stock legs when doing short sale
  , _comShortSaleSlot :: Int  -- 1 = clearing broker, 2 = third party
  , _comDesignatedLocation :: String
  , _comExemptCode :: Int
  } deriving Show

parseIBComboLeg :: Parser IBComboLeg
parseIBComboLeg = 
  IBComboLeg <$>
    parseIntField <*>
    parseIntField <*>
    parseStringField <*>
    parseStringField <*>
    parseIntField <*>
    parseIntField <*>
    parseStringField <*>
    parseIntField   

-- -----------------------------------------------------------------------------
-- Under comp - TBC

data IBUnderComp = IBUnderComp 
  { _ucConId :: Int
  , _ucDelta :: Double
  , _ucPrice :: Double
  } deriving Show

parseIBUnderComp :: Parser IBUnderComp
parseIBUnderComp = 
  IBUnderComp <$>
    parseIntField <*>
    parseSignedDoubleField <*>
    parseDoubleField

-- -----------------------------------------------------------------------------
-- Contract details

data IBContractDetails = IBContractDetails 
  { _cdSummary :: IBContract
  , _cdMarketName :: String
  , _cdMinTick :: Double
  , _cdPriceMagnifier :: Int
  , _cdOrderTypes :: String
  , _cdValidExchanges :: String
  , _cdUnderConId :: Int
  , _cdLongName :: String
  , _cdContractMonth :: String
  , _cdIndustry :: String
  , _cdCategory :: String
  , _cdSubCategory :: String
  , _cdTimeZoneId :: String
  , _cdTradingHours :: String
  , _cdLiquidHours :: String
  , _cdEvRule :: String
  , _cdEvMultiplier :: Double
  , _cdSecIds :: IBTagValues
  } deriving Show 

newIBContractDetails :: IBContractDetails
newIBContractDetails = IBContractDetails 
  { _cdSummary = newIBContract
  , _cdMarketName = ""
  , _cdMinTick = 0
  , _cdPriceMagnifier = 0
  , _cdOrderTypes = ""
  , _cdValidExchanges = ""
  , _cdUnderConId = 0
  , _cdLongName = ""
  , _cdContractMonth = ""
  , _cdIndustry = ""
  , _cdCategory = ""
  , _cdSubCategory = ""
  , _cdTimeZoneId = ""
  , _cdTradingHours = ""
  , _cdLiquidHours = ""
  , _cdEvRule = ""
  , _cdEvMultiplier = 0
  , _cdSecIds = Map.empty
  }

-- -----------------------------------------------------------------------------
-- Combo leg

data IBOrderComboLeg = IBOrderComboLeg 
  { _oclPrice :: Double 
  } deriving Show

parseIBOrderComboLeg :: Parser IBOrderComboLeg
parseIBOrderComboLeg = 
  IBOrderComboLeg <$> 
    parseDoubleField'

type IBOrderComboLegs = [IBOrderComboLeg]

-- -----------------------------------------------------------------------------
-- Order

data IBOrder = IBOrder 
  -- Main order fields
  { _orderId                             :: ByteString                     
  , _orderClientId                       :: Int 
  , _orderPermId                         :: Maybe Int                      -- Populated in open order message (permanent id for subitted and open orders, not used for new order?)
  , _orderAction                         :: IBOrderAction                  -- BUY, SELL, ?
  , _orderTotalQuantity                  :: Int                           
  , _orderType                           :: IBOrderType                    -- e.g. MKT, LMT
  , _orderLmtPrice                       :: Maybe Double                  
  , _orderAuxPrice                       :: Maybe Double                  
  -- Extended order fields                                              
  , _orderTif                            :: IBOrderTimeInForce             -- "Time in Force" - DAY, GTC, etc.
  , _orderOcaGroup                       :: Maybe String                   -- one cancels all group name
  , _orderOcaType                        :: Maybe IBOrderOCAType           -- 1 = CANCEL_WITH_BLOCK, 2 = REDUCE_WITH_BLOCK, 3 = REDUCE_NON_BLOCK
  , _orderRef                            :: String
  , _orderTransmit                       :: Bool                           -- if false, order will be created but not transmited
  , _orderParentId                       :: Maybe Int                      -- Parent order Id, to associate Auto STP or TRAIL orders with the original order.
  , _orderBlockOrder                     :: Bool                            
  , _orderSweepToFill                    :: Bool                            
  , _orderDisplaySize                    :: Maybe Int                      -- TBC       
  , _orderTriggerMethod                  :: IBOrderTriggerMethod           -- 0=Default, 1=Double_Bid_Ask, 2=Last, 3=Double_Last, 4=Bid_Ask, 7=Last_or_Bid_Ask, 8=Mid-point
  , _orderOutsideRth                     :: Bool                            
  , _orderHidden                         :: Bool                            
  , _orderGoodAfterTime                  :: Maybe String                   -- FORMAT: 20060505 08:00:00 {time zone}
  , _orderGoodTillDate                   :: Maybe String                   -- FORMAT: 20060505 08:00:00 {time zone}
  , _orderOverridePercentageConstraints  :: Bool                           
  , _orderRule80A                        :: Maybe IBOrderRule80A           -- Individual = 'I', Agency = 'A', AgentOtherMember = 'W', IndividualPTIA = 'J', AgencyPTIA = 'U', AgentOtherMemberPTIA = 'M', IndividualPT = 'K', AgencyPT = 'Y', AgentOtherMemberPT = 'N'
  , _orderAllOrNone                      :: Bool                            
  , _orderMinQty                         :: Maybe Int                             
  , _orderPercentOffset                  :: Maybe Double                   -- REL orders only; specify the decimal, e.g. .04 not 4
  , _orderTrailStopPrice                 :: Maybe Double                   -- for TRAILLIMIT orders only
  , _orderTrailingPercent                :: Maybe Double                   -- specify the percentage, e.g. 3, not .03
  -- Financial advisors only                                      
  , _orderFAGroup                        :: String                   
  , _orderFAProfile                      :: String                   
  , _orderFAMethod                       :: String                   
  , _orderFAPercentage                   :: String                   
  -- Institutional orders only                                    
  , _orderOpenClose                      :: IBOrderOpenClose               -- O=Open, C=Close
  , _orderOrigin                         :: IBOrderOrigin                  -- 0=Customer, 1=Firm
  , _orderShortSaleSlot                  :: Maybe IBOrderShortSaleSlot     -- 1 if you hold the shares, 2 if they will be delivered from elsewhere.  Only for Action="SSHORT
  , _orderDesignatedLocation             :: Maybe String                   -- set when slot=2 only.
  , _orderExemptCode                     :: Int                            -- TBC
  -- SMART routing only                                           
  , _orderDiscretionaryAmt               :: Double                          
  , _orderETradeOnly                     :: Bool                    
  , _orderFirmQuoteOnly                  :: Bool                      
  , _orderNBBOPriceCap                   :: Maybe Double                      
  , _orderOptOutSmartRouting             :: Bool                          
  -- Box or vol orders only                                       
  , _orderAuctionStrategy                :: Maybe IBOrderAuctionStrategy   -- 1=AUCTION_MATCH, 2=AUCTION_IMPROVEMENT, 3=AUCTION_TRANSPARENT
  -- Box orders only                                              
  , _orderStartingPrice                  :: Maybe Double                   
  , _orderStockRefPrice                  :: Maybe Double                   
  , _orderDelta                          :: Maybe Double                   
  -- Pegged to stock or vol orders                                
  , _orderStockRangeLower                :: Maybe Double                   
  , _orderStockRangeUpper                :: Maybe Double                   
  -- Volatility orders only                                        
  , _orderVolatility                     :: Maybe Double                   -- Enter percentage not decimal, e.g. 2 not .02
  , _orderVolatilityType                 :: Maybe IBOrderVolatilityType    -- 1=daily, 2=annual
  , _orderContinuousUpdate               :: Maybe Int                      -- TBC
  , _orderReferencePriceType             :: Maybe IBOrderRefPriceType      -- 1=Bid/Ask midpoint, 2 = BidOrAsk
  , _orderDeltaNeutralOrderType          :: Maybe String                   -- TBC             
  , _orderDeltaNeutralAuxPrice           :: Maybe Double                    
  , _orderDeltaNeutralConId              :: Maybe Int                      
  , _orderDeltaNeutralSettlingFirm       :: Maybe String                   
  , _orderDeltaNeutralClearingAccount    :: Maybe String                     
  , _orderDeltaNeutralClearingIntent     :: Maybe String                      
  , _orderDeltaNeutralOpenClose          :: Maybe IBOrderOpenClose         -- TBC         
  , _orderDeltaNeutralShortSale          :: Bool                    
  , _orderDeltaNeutralShortSaleSlot      :: Maybe IBOrderShortSaleSlot     -- TBC                       
  , _orderDeltaNeutralDesignatedLocation :: Maybe String                       
  -- Combo orders only                                            
  , _orderBasisPoints                    :: Maybe Double                   -- TBC. EFP orders only, download only
  , _orderBasisPointsType                :: Maybe Int                      -- TBC. EFP orders only, download only
  -- Scale orders only                                            
  , _orderScaleInitLevelSize             :: Maybe Int                      
  , _orderScaleSubsLevelSize             :: Maybe Int                      
  , _orderScalePriceIncrement            :: Maybe Double                   
  , _orderScalePriceAdjustValue          :: Maybe Double                   
  , _orderScalePriceAdjustInterval       :: Maybe Int                        
  , _orderScaleProfitOffset              :: Maybe Double                   
  , _orderScaleAutoReset                 :: Bool                    
  , _orderScaleInitPosition              :: Maybe Int                      
  , _orderScaleInitFillQty               :: Maybe Int                      
  , _orderScaleRandomPercent             :: Bool                    
  -- Hedge orders only                                            
  , _orderHedgeType                      :: Maybe IBOrderHedgeType         -- 'D' - delta, 'B' - beta, 'F' - FX, 'P' - pair
  , _orderHedgeParam                     :: Maybe String                   -- TBC. beta value for beta hedge (in range 0-1), ratio for pair hedge
  -- Clearing info                                                
  , _orderAccount                        :: String                         -- IB account
  , _orderSettlingFirm                   :: String                   
  , _orderClearingAccount                :: String                         -- True beneficiary of the order
  , _orderClearingIntent                 :: String                         -- "" (Default), "IB", "Away", "PTA" (PostTrade)
  -- Algo orders only                                             
  , _orderAlgoStrategy                   :: Maybe String                   
  , _orderAlgoParams                     :: IBTagValues              
  -- What-if                                                      
  , _orderWhatIf                         :: Bool                    
  -- Not Held                                                     
  , _orderNotHeld                        :: Bool                    
  -- Smart combo routing params                                   
  , _orderSmartComboRoutingParams        :: IBTagValues             
  -- Order combo legs                                             
  , _orderComboLegs                      :: IBOrderComboLegs 
  } deriving Show

newIBOrder :: IBOrder
newIBOrder = IBOrder 
  { _orderId                             = ""                    
  , _orderClientId                       = 0                    
  , _orderPermId                         = Nothing              
  , _orderAction                         = Buy          
  , _orderTotalQuantity                  = 0                    
  , _orderType                           = Market            
  , _orderLmtPrice                       = Nothing           
  , _orderAuxPrice                       = Nothing           
  , _orderTif                            = Day    
  , _orderOcaGroup                       = Nothing                 
  , _orderOcaType                        = Nothing
  , _orderRef                            = ""                 
  , _orderTransmit                       = False                   
  , _orderParentId                       = Nothing              
  , _orderBlockOrder                     = False                   
  , _orderSweepToFill                    = False                   
  , _orderDisplaySize                    = Nothing              
  , _orderTriggerMethod                  = TriggerDefault       
  , _orderOutsideRth                     = False                   
  , _orderHidden                         = False                   
  , _orderGoodAfterTime                  = Nothing           
  , _orderGoodTillDate                   = Nothing           
  , _orderOverridePercentageConstraints  = False                   
  , _orderRule80A                        = Nothing        
  , _orderAllOrNone                      = False                   
  , _orderMinQty                         = Nothing              
  , _orderPercentOffset                  = Nothing           
  , _orderTrailStopPrice                 = Nothing           
  , _orderTrailingPercent                = Nothing           
  , _orderFAGroup                        = ""                 
  , _orderFAProfile                      = ""                 
  , _orderFAMethod                       = ""                 
  , _orderFAPercentage                   = ""                 
  , _orderOpenClose                      = OrderOpen            
  , _orderOrigin                         = Customer               
  , _orderShortSaleSlot                  = Nothing  
  , _orderDesignatedLocation             = Nothing           
  , _orderExemptCode                     = -1                    
  , _orderDiscretionaryAmt               = 0                 
  , _orderETradeOnly                     = False                   
  , _orderFirmQuoteOnly                  = False                   
  , _orderNBBOPriceCap                   = Nothing           
  , _orderOptOutSmartRouting             = False                   
  , _orderAuctionStrategy                = Nothing
  , _orderStartingPrice                  = Nothing           
  , _orderStockRefPrice                  = Nothing           
  , _orderDelta                          = Nothing           
  , _orderStockRangeLower                = Nothing           
  , _orderStockRangeUpper                = Nothing           
  , _orderVolatility                     = Nothing           
  , _orderVolatilityType                 = Nothing 
  , _orderContinuousUpdate               = Nothing              
  , _orderReferencePriceType             = Nothing   
  , _orderDeltaNeutralOrderType          = Nothing           
  , _orderDeltaNeutralAuxPrice           = Nothing           
  , _orderDeltaNeutralConId              = Nothing              
  , _orderDeltaNeutralSettlingFirm       = Nothing           
  , _orderDeltaNeutralClearingAccount    = Nothing           
  , _orderDeltaNeutralClearingIntent     = Nothing           
  , _orderDeltaNeutralOpenClose          = Nothing           
  , _orderDeltaNeutralShortSale          = False                   
  , _orderDeltaNeutralShortSaleSlot      = Nothing  
  , _orderDeltaNeutralDesignatedLocation = Nothing           
  , _orderBasisPoints                    = Nothing           
  , _orderBasisPointsType                = Nothing              
  , _orderScaleInitLevelSize             = Nothing              
  , _orderScaleSubsLevelSize             = Nothing              
  , _orderScalePriceIncrement            = Nothing           
  , _orderScalePriceAdjustValue          = Nothing           
  , _orderScalePriceAdjustInterval       = Nothing              
  , _orderScaleProfitOffset              = Nothing           
  , _orderScaleAutoReset                 = False                   
  , _orderScaleInitPosition              = Nothing              
  , _orderScaleInitFillQty               = Nothing              
  , _orderScaleRandomPercent             = False                   
  , _orderHedgeType                      = Nothing      
  , _orderHedgeParam                     = Nothing           
  , _orderAccount                        = ""                 
  , _orderSettlingFirm                   = ""                 
  , _orderClearingAccount                = ""                 
  , _orderClearingIntent                 = ""                 
  , _orderAlgoStrategy                   = Nothing           
  , _orderAlgoParams                     = Map.empty            
  , _orderWhatIf                         = False                   
  , _orderNotHeld                        = False                   
  , _orderSmartComboRoutingParams        = Map.empty            
  , _orderComboLegs                      = []
  }

parseIBOrder :: IBContract -> Parser (IBOrder,IBContract)
parseIBOrder contract = do
  let order = newIBOrder
  orderAction' <- parseField parseIBOrderAction
  orderTotalQuantity' <- parseIntField
  orderType' <- parseField parseIBOrderType
  orderLmtPrice' <- parseMaybeDoubleField
  orderAuxPrice' <- parseMaybeDoubleField
  orderTif' <- parseField parseIBOrderTimeInForce
  orderOcaGroup' <- parseMaybe parseStringField
  orderAccount' <- parseStringField
  orderOpenClose' <- parseField parseIBOrderOpenClose
  orderOrigin' <- parseField parseIBOrderOrigin
  orderRef' <- parseStringField
  orderClientId' <- parseIntField
  orderPermId' <- Just <$> parseIntField
  orderOutsideRth' <- parseBoolBinaryField
  orderHidden' <- parseBoolBinaryField
  orderDiscretionaryAmt' <- parseDoubleField'
  orderGoodAfterTime' <- parseMaybeStringField
  _ <- parseStringField
  orderFAGroup' <- parseStringField
  orderFAProfile' <- parseStringField
  orderFAMethod' <- parseStringField
  orderFAPercentage' <- parseStringField
  orderGoodTillDate' <- parseMaybeStringField
  orderRule80A' <- parseMaybe $ parseField parseIBOrderRule80A
  orderPercentOffset' <- parseMaybeDoubleField
  orderSettlingFirm' <- parseStringField
  orderShortSaleSlot' <- parseMaybe $ parseField parseIBOrderShortSaleSlot
  orderDesignatedLocation' <- parseMaybeStringField  
  orderExemptCode' <- parseSignedIntField
  orderAuctionStrategy' <- parseMaybe $ parseField parseIBOrderAuctionStrategy
  orderStartingPrice' <- parseMaybeDoubleField
  orderStockRefPrice' <- parseMaybeDoubleField
  orderDelta' <- parseMaybeDoubleField
  orderStockRangeLower' <- parseMaybeDoubleField
  orderStockRangeUpper' <- parseMaybeDoubleField
  orderDisplaySize' <- parseMaybeIntField
  orderBlockOrder' <- parseBoolBinaryField
  orderSweepToFill' <- parseBoolBinaryField
  orderAllOrNone' <- parseBoolBinaryField
  orderMinQty' <- parseMaybeIntField
  orderOcaType' <- parseMaybe $ parseField parseIBOrderOCAType
  orderETradeOnly' <- parseBoolBinaryField
  orderFirmQuoteOnly' <- parseBoolBinaryField
  orderNBBOPriceCap' <- parseMaybeDoubleField
  orderParentId' <- parseMaybeIntField
  orderTriggerMethod' <- parseField parseIBOrderTriggerMethod
  orderVolatility' <- parseMaybeDoubleField
  orderVolatilityType' <- parseMaybe $ parseField parseIBOrderVolatilityType
  orderDeltaNeutralOrderType'' <- parseStringField
  let dn' = not (null orderDeltaNeutralOrderType'')
  let orderDeltaNeutralOrderType' = if dn' then Just orderDeltaNeutralOrderType'' else Nothing
  orderDeltaNeutralAuxPrice' <- parseMaybeDoubleField
  orderDeltaNeutralConId' <- if dn' then parseMaybeIntField else return $ _orderDeltaNeutralConId order
  orderDeltaNeutralSettlingFirm' <- if dn' then parseMaybeStringField else return $ _orderDeltaNeutralSettlingFirm order
  orderDeltaNeutralClearingAccount' <- if dn' then parseMaybeStringField else return $ _orderDeltaNeutralClearingAccount order
  orderDeltaNeutralClearingIntent' <- if dn' then parseMaybeStringField else return $ _orderDeltaNeutralClearingIntent order
  orderDeltaNeutralOpenClose' <- if dn' then parseMaybe (parseField parseIBOrderOpenClose) else return $ _orderDeltaNeutralOpenClose order
  orderDeltaNeutralShortSale' <- if dn' then parseBoolBinaryField else return $ _orderDeltaNeutralShortSale order
  orderDeltaNeutralShortSaleSlot' <- if dn' then parseMaybe (parseField parseIBOrderShortSaleSlot) else return $ _orderDeltaNeutralShortSaleSlot order
  orderDeltaNeutralDesignatedLocation' <- if dn' then parseMaybeStringField else return $ _orderDeltaNeutralDesignatedLocation order
  orderContinuousUpdate' <- parseMaybeIntField
  orderReferencePriceType' <- parseMaybe $ parseField parseIBOrderRefPriceType
  orderTrailStopPrice' <- parseMaybeDoubleField
  orderTrailingPercent' <- parseMaybeDoubleField
  orderBasisPoints' <- parseMaybeDoubleField
  orderBasisPointsType' <- parseMaybeIntField
  contract' <- parseIBContractComboLegs contract 
  orderComboLegsCount' <- parseIntField
  orderComboLegs' <- replicateM orderComboLegsCount' parseIBOrderComboLeg
  orderSmartComboRoutingParamsCount' <- parseIntField
  orderSmartComboRoutingParams' <- if orderSmartComboRoutingParamsCount' > 0 then parseTagValues else return $ _orderSmartComboRoutingParams order
  orderScaleInitLevelSize' <- parseMaybeIntField
  orderScaleSubsLevelSize' <- parseMaybeIntField
  orderScalePriceIncrement' <- parseMaybeDoubleField
  let scale' = fmap (>0) orderScalePriceIncrement' == Just True 
  orderScalePriceAdjustValue' <- if scale' then parseMaybeDoubleField else return $ _orderScalePriceAdjustValue order
  orderScalePriceAdjustInterval' <- if scale' then parseMaybeIntField else return $ _orderScalePriceAdjustInterval order
  orderScaleProfitOffset' <- if scale' then parseMaybeDoubleField else return $ _orderScaleProfitOffset order
  orderScaleAutoReset' <- if scale' then parseBoolBinaryField else return $ _orderScaleAutoReset order
  orderScaleInitPosition' <- if scale' then parseMaybeIntField else return $ _orderScaleInitPosition order
  orderScaleInitFillQty' <- if scale' then parseMaybeIntField else return $ _orderScaleInitFillQty order
  orderScaleRandomPercent' <- if scale' then parseBoolBinaryField else return $ _orderScaleRandomPercent order
  orderHedgeType' <- parseMaybe $ parseField parseIBOrderHedgeType
  orderHedgeParam' <- if isJust orderHedgeType' then parseMaybeStringField else return $ _orderHedgeParam order
  orderOptOutSmartRouting' <- parseBoolBinaryField
  orderClearingAccount' <- parseStringField
  orderClearingIntent' <- parseStringField
  orderNotHeld' <- parseBoolBinaryField
  contract'' <- parseIBContractUnderComp contract' 
  orderAlgoStrategy' <- parseMaybeStringField
  orderAlgoParams' <- if isJust orderAlgoStrategy' then parseTagValues else return $ _orderAlgoParams order
  orderWhatIf' <- parseBoolBinaryField
  return (
    order 
      { _orderClientId                       = orderClientId'
      , _orderPermId                         = orderPermId'
      , _orderAction                         = orderAction'
      , _orderTotalQuantity                  = orderTotalQuantity'
      , _orderType                           = orderType'
      , _orderLmtPrice                       = orderLmtPrice'
      , _orderAuxPrice                       = orderAuxPrice'
      , _orderTif                            = orderTif'
      , _orderOcaGroup                       = orderOcaGroup'
      , _orderOcaType                        = orderOcaType'
      , _orderRef                            = orderRef'
       --orderTransmit                       = orderTransmit'
      , _orderParentId                       = orderParentId'
      , _orderBlockOrder                     = orderBlockOrder'
      , _orderSweepToFill                    = orderSweepToFill'
      , _orderDisplaySize                    = orderDisplaySize'
      , _orderTriggerMethod                  = orderTriggerMethod'
      , _orderOutsideRth                     = orderOutsideRth'
      , _orderHidden                         = orderHidden'
      , _orderGoodAfterTime                  = orderGoodAfterTime'
      , _orderGoodTillDate                   = orderGoodTillDate'
       --orderOverridePercentageConstraints  = orderOverridePercentageConstraints'
      , _orderRule80A                        = orderRule80A'
      , _orderAllOrNone                      = orderAllOrNone'                     
      , _orderMinQty                         = orderMinQty'
      , _orderPercentOffset                  = orderPercentOffset'
      , _orderTrailStopPrice                 = orderTrailStopPrice'
      , _orderTrailingPercent                = orderTrailingPercent'
      , _orderFAGroup                        = orderFAGroup'
      , _orderFAProfile                      = orderFAProfile'
      , _orderFAMethod                       = orderFAMethod'
      , _orderFAPercentage                   = orderFAPercentage'
      , _orderOpenClose                      = orderOpenClose'
      , _orderOrigin                         = orderOrigin'
      , _orderShortSaleSlot                  = orderShortSaleSlot'
      , _orderDesignatedLocation             = orderDesignatedLocation'
      , _orderExemptCode                     = orderExemptCode'
      , _orderDiscretionaryAmt               = orderDiscretionaryAmt'
      , _orderETradeOnly                     = orderETradeOnly'
      , _orderFirmQuoteOnly                  = orderFirmQuoteOnly'
      , _orderNBBOPriceCap                   = orderNBBOPriceCap'
      , _orderOptOutSmartRouting             = orderOptOutSmartRouting'
      , _orderAuctionStrategy                = orderAuctionStrategy'
      , _orderStartingPrice                  = orderStartingPrice'
      , _orderStockRefPrice                  = orderStockRefPrice'
      , _orderDelta                          = orderDelta'
      , _orderStockRangeLower                = orderStockRangeLower'
      , _orderStockRangeUpper                = orderStockRangeUpper'
      , _orderVolatility                     = orderVolatility'
      , _orderVolatilityType                 = orderVolatilityType'
      , _orderContinuousUpdate               = orderContinuousUpdate'
      , _orderReferencePriceType             = orderReferencePriceType'
      , _orderDeltaNeutralOrderType          = orderDeltaNeutralOrderType'
      , _orderDeltaNeutralAuxPrice           = orderDeltaNeutralAuxPrice'
      , _orderDeltaNeutralConId              = orderDeltaNeutralConId'
      , _orderDeltaNeutralSettlingFirm       = orderDeltaNeutralSettlingFirm'
      , _orderDeltaNeutralClearingAccount    = orderDeltaNeutralClearingAccount'
      , _orderDeltaNeutralClearingIntent     = orderDeltaNeutralClearingIntent'
      , _orderDeltaNeutralOpenClose          = orderDeltaNeutralOpenClose'
      , _orderDeltaNeutralShortSale          = orderDeltaNeutralShortSale'
      , _orderDeltaNeutralShortSaleSlot      = orderDeltaNeutralShortSaleSlot'
      , _orderDeltaNeutralDesignatedLocation = orderDeltaNeutralDesignatedLocation'
      , _orderBasisPoints                    = orderBasisPoints'
      , _orderBasisPointsType                = orderBasisPointsType'
      , _orderScaleInitLevelSize             = orderScaleInitLevelSize'
      , _orderScaleSubsLevelSize             = orderScaleSubsLevelSize'
      , _orderScalePriceIncrement            = orderScalePriceIncrement'
      , _orderScalePriceAdjustValue          = orderScalePriceAdjustValue'
      , _orderScalePriceAdjustInterval       = orderScalePriceAdjustInterval'
      , _orderScaleProfitOffset              = orderScaleProfitOffset'
      , _orderScaleAutoReset                 = orderScaleAutoReset'
      , _orderScaleInitPosition              = orderScaleInitPosition'
      , _orderScaleInitFillQty               = orderScaleInitFillQty'
      , _orderScaleRandomPercent             = orderScaleRandomPercent'
      , _orderHedgeType                      = orderHedgeType'
      , _orderHedgeParam                     = orderHedgeParam'
      , _orderAccount                        = orderAccount'
      , _orderSettlingFirm                   = orderSettlingFirm'
      , _orderClearingAccount                = orderClearingAccount'
      , _orderClearingIntent                 = orderClearingIntent'
      , _orderAlgoStrategy                   = orderAlgoStrategy'
      , _orderAlgoParams                     = orderAlgoParams'
      , _orderWhatIf                         = orderWhatIf'
      , _orderNotHeld                        = orderNotHeld'
      , _orderSmartComboRoutingParams        = orderSmartComboRoutingParams'
      , _orderComboLegs                      = orderComboLegs'
      },
    contract'')

-- -----------------------------------------------------------------------------
-- Order state

data IBOrderState = IBOrderState
  { _osStatus :: IBOrderStatus
  , _osInitMargin :: String
  , _osMaintMargin :: String
  , _osEquityWithLoan :: String
  , _osCommission :: Double
  , _osMinCommission :: Double
  , _osMaxCommission :: Double
  , _osCommissionCurrency :: String
  , _osWarningText :: String
  } deriving Show

newIBOrderState :: IBOrderState
newIBOrderState = IBOrderState 
  { _osStatus = Inactive
  , _osInitMargin = ""
  , _osMaintMargin = ""
  , _osEquityWithLoan = ""
  , _osCommission = 0.0
  , _osMinCommission = 0.0
  , _osMaxCommission = 0.0
  , _osCommissionCurrency = ""
  , _osWarningText = ""
  }

parseIBOrderState :: Parser IBOrderState
parseIBOrderState = 
  IBOrderState <$>
    parseField parseIBOrderStatus <*>
    parseStringField <*>
    parseStringField <*>
    parseStringField <*>
    parseDoubleField' <*> 
    parseDoubleField' <*>
    parseDoubleField' <*>
    parseStringField <*>
    parseStringField

-- -----------------------------------------------------------------------------
-- Execution

data IBExecution = IBExecution 
  { _execOrderId      :: ByteString        
  , _execClientId     :: Int     
  , _execId           :: ByteString
  , _execTime         :: LocalTime --String
  , _execAcctNumber   :: String     
  , _execExchange     :: String   
  , _execSide         :: IBExecutionSide --String
  , _execShares       :: Int  
  , _execPrice        :: Double
  , _execPermId       :: Int  
  , _execLiquidation  :: Int         
  , _execCumQty       :: Int    
  , _execAvgPrice     :: Double
  , _execOrderRef     :: String  
  , _execEvRule       :: String 
  , _execEvMultiplier :: Double       
  } deriving Show

newIBExecution :: IBExecution
newIBExecution = IBExecution 
  { _execOrderId      = ""    
  , _execClientId     = 0
  , _execId           = "" 
  , _execTime         = zeroTimeLocal
  , _execAcctNumber   = ""     
  , _execExchange     = ""   
  , _execSide         = Bought
  , _execShares       = 0    
  , _execPrice        = 0.0
  , _execPermId       = 0   
  , _execLiquidation  = 0         
  , _execCumQty       = 0    
  , _execAvgPrice     = 0.0   
  , _execOrderRef     = ""  
  , _execEvRule       = "" 
  , _execEvMultiplier = 0.0 
  }

-- -----------------------------------------------------------------------------
-- Commission report

data IBCommissionReport = IBCommissionReport 
  { _crExecId :: ByteString
  , _crCommission :: Double
  , _crCurrency :: Currency
  , _crRealisedPNL :: Double
  , _crYield :: Double
  , _crYieldRedemptionDate :: Int 
  } deriving Show

newIBCommissionReport :: IBCommissionReport
newIBCommissionReport = IBCommissionReport 
  { _crExecId = ""
  , _crCommission = 0.0
  , _crCurrency = "USD"
  , _crRealisedPNL = 0.0
  , _crYield = 0.0
  , _crYieldRedemptionDate = 0  
  }

parseIBCommissionReport :: Parser IBCommissionReport
parseIBCommissionReport =
  IBCommissionReport <$>
    parseByteStringField <*>
    parseDoubleField <*>
    parseField parseStringToEnum <*>
    parseDoubleField <*>
    parseDoubleField <*>
    parseIntField'

-- -----------------------------------------------------------------------------
-- Execution filter

data IBExecutionFilter = IBExecutionFilter 
  { _efClientId :: Int             -- zero means no filtering on this field
  , _efAcctCode :: String
  , _efTime :: String
  , _efSymbol :: String
  , _efSecType :: IBSecurityType -- String, TODO: Maybe? Check if blank means no filter
  , _efExchange :: String
  , _efSide :: String --TODO: enum?
  } deriving Show

newIBExecutionFilter :: IBExecutionFilter
newIBExecutionFilter = IBExecutionFilter 
  { _efClientId = 0              -- zero means no filtering on this field
  , _efAcctCode = ""
  , _efTime = ""
  , _efSymbol = ""
  , _efSecType = IBFuture 
  , _efExchange = ""
  , _efSide = ""  
  }

-- -----------------------------------------------------------------------------
-- Lenses

makeLenses ''IBRequest
makeLenses ''IBHistoricalDataItem
makeLenses ''IBResponse
makeLenses ''IBContract
makeLenses ''IBComboLeg
makeLenses ''IBUnderComp
makeLenses ''IBContractDetails
makeLenses ''IBOrderComboLeg
makeLenses ''IBOrder
makeLenses ''IBOrderState
makeLenses ''IBExecution
makeLenses ''IBCommissionReport
makeLenses ''IBExecutionFilter
