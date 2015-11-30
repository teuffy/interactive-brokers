{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module API.IB.Request where

import           Data.ByteString.Char8              (ByteString)
import qualified Data.ByteString.Char8              as BC
import           Data.ByteString.Lazy.Builder       (Builder, stringUtf8)
import           Data.ByteString.Lazy.Builder.ASCII (intDec)
import qualified Data.Map                           as Map (size, toList)
import           Data.Maybe
import           Data.Monoid                        hiding (All, Product)
import           Data.String
import           Data.Time                          --hiding (defaultTimeLocale)
import           Prelude                            hiding (takeWhile)
import           System.Locale                      --hiding (defaultTimeLocale)

import           API.IB.Constant
import           API.IB.Data
import           API.IB.Enum
import           API.IB.Util

-----------------------------------------------------------------------------
-- Types

type Msg = Either MsgError ByteString

data MsgError = 
    InvalidBarSize
  | InvalidBarBasis
  | InvalidDuration
  | InvalidServerVersion
    deriving (Eq,Enum)

instance Show MsgError where
  show InvalidBarSize = "Invalid bar size"
  show InvalidBarBasis = "Invalid bar basis"
  show InvalidDuration = "Invalid duration"
  show InvalidServerVersion = "Invalid server version"

-- -----------------------------------------------------------------------------
-- Utilities

ibMsg :: Int -> IBRequestType -> [Builder] -> ByteString
ibMsg version reqtype builders = bMake sepC $ bEncode reqtype : intDec version : builders

ibMsgConcat :: Int -> IBRequestType -> [[Builder]] -> ByteString
ibMsgConcat version reqtype builders = ibMsg version reqtype $ concat builders   

ibFormatDay :: FormatTime t => t -> String
ibFormatDay = formatTime defaultTimeLocale "%Y%m%d"

ibFormatTime :: FormatTime t => t -> String
ibFormatTime = formatTime defaultTimeLocale "%Y%m%d %T"

-- -----------------------------------------------------------------------------
-- Message Factory

createClientIdMsg :: Int -> ByteString
createClientIdMsg clientid = BC.pack (show clientid) <> sepB

createClientVersionMsg :: Int -> ByteString
createClientVersionMsg cversion = BC.pack (show cversion) <> sepB

createMsg :: IBRequest -> Msg
createMsg request = case request of 
  (RequestMarketData sv tid c gtl ss) -> createRequestMarketDataMsg sv tid c gtl ss
  (CancelMarketData tid) -> createCancelMarketDataMsg tid
  (PlaceOrder sv oid c o) -> createPlaceOrderMsg sv oid c o
  (CancelOrder oid) -> createCancelOrderMsg oid 
  RequestOpenOrders -> createRequestOpenOrdersMsg
  (RequestAccountData sv sub ac) -> createRequestAccountDataMsg sv sub ac
  (RequestExecutions rid ef) -> createRequestExecutionsMsg rid ef 
  (RequestIds n) -> createRequestIdsMsg n
  (RequestContractData sv rid c) -> createRequestContractDataMsg sv rid c
  (RequestAutoOpenOrders ab) -> createRequestAutoOpenOrdersMsg ab
  RequestAllOpenOrders -> createRequestAllOpenOrdersMsg
  RequestManagedAccounts -> createRequestManagedAccountsMsg
  (RequestHistoricalData sv tid c dt dur bs bb rth fd) -> createRequestHistoricalDataMsg sv tid c dt dur bs bb rth fd
  (CancelHistoricalData tid) -> createCancelHistoricalDataMsg tid
  RequestCurrentTime -> createRequestCurrentTimeMsg
  (RequestRealTimeBars sv tid c bs bb rth) -> createRequestRealTimeBarsMsg sv tid c bs bb rth
  (CancelRealTimeBars tid) -> createCancelRealTimeBarsMsg tid
  RequestGlobalCancel -> createRequestGlobalCancelMsg
  (RequestMarketDataType mdt) -> createRequestMarketDataTypeMsg mdt
  RequestPositions -> createRequestPositionsMsg
  (RequestAccountSummary sv rid g tl) -> createRequestAccountSummaryMsg sv rid g tl
  (CancelAccountSummary sv rid) -> createCancelAccountSummaryMsg sv rid
  (CancelPositions sv) -> createCancelPositionsMsg sv

-- -----------------------------------------------------------------------------

createRequestMarketDataMsg :: Int -> Int -> IBContract -> [IBGenericTickType] -> Bool -> Msg
createRequestMarketDataMsg sversion tickerid IBContract{..} genticktypes snapshot = 
  return $ ibMsgConcat 10 ReqMktDataT
    [ [ intDec tickerid
      , intDec _conId
      , stringUtf8 _conSymbol
      , bEncode _conSecType
      , stringUtf8 (maybe "" ibFormatDay _conExpiry)
      , buildDecimal _conStrike
      , stringUtf8 (maybe "" encode _conRight)
      , maybe bEmpty buildDecimal _conMultiplier
      , bEncode _conExchange
      , bEncode _conPrimaryExch
      , stringUtf8 (show _conCurrency)
      , stringUtf8 _conLocalSymbol
      ]
    , [ stringUtf8 _conTradingClass | sversion >= minServerVersionTradingClass]
    , [ stringUtf8 "0"
      , bSep ',' (map bEncode genticktypes) 
      , intDec $ boolBinary snapshot
      ]
    ]

-- -----------------------------------------------------------------------------

createCancelMarketDataMsg :: Int -> Msg
createCancelMarketDataMsg tickerid = return $ ibMsg 1 CancelMktDataT [intDec tickerid]

-- -----------------------------------------------------------------------------

createPlaceOrderMsg :: Int -> Int -> IBContract -> IBOrder -> Msg
createPlaceOrderMsg sversion orderid IBContract{..} IBOrder{..} =
  return $ ibMsgConcat 40 PlaceOrderT
    [ [ intDec orderid
      , intDec _conId
      , stringUtf8 _conSymbol
      , bEncode _conSecType
      , stringUtf8 (maybe "" ibFormatDay _conExpiry)
      , buildDecimal _conStrike
      , stringUtf8 (maybe "" encode _conRight)
      , maybe bEmpty buildDecimal _conMultiplier
      , bEncode _conExchange
      , bEncode _conPrimaryExch
      , stringUtf8 (show _conCurrency)
      , stringUtf8 _conLocalSymbol
      ]
    , [ stringUtf8 _conTradingClass | sversion >= minServerVersionTradingClass
      ]
    , [ stringUtf8 (maybe "" encode _conSecIdType)
      , stringUtf8 _conSecId
      ]
    , [ bEncode _orderAction
      , intDec _orderTotalQuantity
      , bEncode _orderType
      , maybe bEmpty buildDecimal _orderLmtPrice
      , maybe bEmpty buildDecimal _orderAuxPrice
      , bEncode _orderTif
      , maybe bEmpty stringUtf8 _orderOcaGroup
      , stringUtf8 _orderAccount
      , bEncode _orderOpenClose
      , bEncode _orderOrigin
      , stringUtf8 _orderRef
      , intDec $ boolBinary _orderTransmit
      , maybe (intDec 0) intDec _orderParentId
      , intDec $ boolBinary _orderBlockOrder
      , intDec $ boolBinary _orderSweepToFill
      , maybe (intDec 0) intDec _orderDisplaySize
      , bEncode _orderTriggerMethod
      , intDec $ boolBinary _orderOutsideRth
      , intDec $ boolBinary _orderHidden
      ]
    , if _conSecType == IBBag then
        let
          _conComboLegsCount = length _conComboLegs
          _orderComboLegsCount = length _orderComboLegs
          _orderSmartComboRoutingParamsCount = Map.size _orderSmartComboRoutingParams
        in
          if _conComboLegsCount > 0 then
            intDec _conComboLegsCount :
            concatMap
              (\l ->   
                [ intDec $ _comConId l
                , intDec $ _comRatio l
                , bEncode $ _comAction l
                , bEncode $ _comExchange l
                , bEncode $ _comOpenClose l
                , bEncode $ _comShortSaleSlot l
                , stringUtf8 $ _comDesignatedLocation l
                , maybe (intDec (-1)) intDec $ _comExemptCode l
                ])
              _conComboLegs 
          else
            [intDec 0]
          <>
          if _orderComboLegsCount > 0 then
            intDec _orderComboLegsCount :
            map
              (buildDecimal . _oclPrice)
              _orderComboLegs
          else
            [intDec 0]
          <>
          if _orderSmartComboRoutingParamsCount > 0 then
            intDec _orderSmartComboRoutingParamsCount :
            concatMap
              (\(tag,value) -> map stringUtf8 [tag,value])
              (Map.toList _orderSmartComboRoutingParams)
          else
            [intDec 0]
      else
        [], 
      [ bEmpty
      , buildDecimal _orderDiscretionaryAmt
      , maybe bEmpty stringUtf8 _orderGoodAfterTime
      , maybe bEmpty stringUtf8 _orderGoodTillDate
      , stringUtf8 _orderFAGroup
      , stringUtf8 _orderFAMethod
      , stringUtf8 _orderFAPercentage
      , stringUtf8 _orderFAProfile
      , maybe bEmpty bEncode _orderShortSaleSlot
      , maybe bEmpty stringUtf8 _orderDesignatedLocation
      , intDec _orderExemptCode
      , maybe bEmpty bEncode _orderOcaType
      , maybe bEmpty bEncode _orderRule80A
      , stringUtf8 _orderSettlingFirm
      , intDec $ boolBinary _orderAllOrNone
      , maybe (intDec 0) intDec _orderMinQty
      , maybe bEmpty buildDecimal _orderPercentOffset
      , intDec $ boolBinary _orderETradeOnly
      , intDec $ boolBinary _orderFirmQuoteOnly
      , maybe bEmpty buildDecimal _orderNBBOPriceCap
      , maybe bEmpty bEncode _orderAuctionStrategy
      , maybe bEmpty buildDecimal _orderStartingPrice
      , maybe bEmpty buildDecimal _orderStockRefPrice
      , maybe bEmpty buildDecimal _orderDelta
      , maybe bEmpty buildDecimal _orderStockRangeLower
      , maybe bEmpty buildDecimal _orderStockRangeUpper
      , intDec $ boolBinary _orderOverridePercentageConstraints
      , maybe bEmpty buildDecimal _orderVolatility
      , maybe bEmpty bEncode _orderVolatilityType
      , maybe bEmpty stringUtf8 _orderDeltaNeutralOrderType
      , maybe bEmpty buildDecimal _orderDeltaNeutralAuxPrice
      ]
    , if isJust _orderDeltaNeutralOrderType then
        [ maybe (intDec 0) intDec _orderDeltaNeutralConId
        , maybe bEmpty stringUtf8 _orderDeltaNeutralSettlingFirm
        , maybe bEmpty stringUtf8 _orderDeltaNeutralClearingAccount
        , maybe bEmpty stringUtf8 _orderDeltaNeutralClearingIntent
        ]
      else
        []
    , if isJust _orderDeltaNeutralOrderType && sversion >= minServerVersionDeltaNeutralOpenClose then
        [ maybe bEmpty bEncode _orderDeltaNeutralOpenClose
        , intDec $ boolBinary _orderDeltaNeutralShortSale
        , maybe bEmpty bEncode _orderDeltaNeutralShortSaleSlot
        , maybe bEmpty stringUtf8 _orderDeltaNeutralDesignatedLocation
        ]
      else
        []
    , [ maybe (intDec 0) intDec _orderContinuousUpdate
      , maybe bEmpty bEncode _orderReferencePriceType
      , maybe bEmpty buildDecimal _orderTrailStopPrice
      , maybe bEmpty buildDecimal _orderTrailingPercent
      , maybe bEmpty intDec _orderScaleInitLevelSize
      , maybe bEmpty intDec _orderScaleSubsLevelSize
      , maybe bEmpty buildDecimal _orderScalePriceIncrement
      ]
    , if isJust _orderScalePriceIncrement && (fromJust _orderScalePriceIncrement > 0) then
        [ maybe bEmpty buildDecimal _orderScalePriceAdjustValue
        , maybe bEmpty intDec _orderScalePriceAdjustInterval
        , maybe bEmpty buildDecimal _orderScaleProfitOffset
        , intDec $ boolBinary _orderScaleAutoReset
        , maybe bEmpty intDec _orderScaleInitPosition
        , maybe bEmpty intDec _orderScaleInitFillQty
        , intDec $ boolBinary _orderScaleRandomPercent
        ]
      else
        []
    , [ maybe bEmpty bEncode _orderHedgeType
      ]
    , if isJust _orderHedgeType then 
        maybe [bEmpty] ((:[]) . stringUtf8) _orderHedgeParam 
      else 
        []
    , [ intDec $ boolBinary _orderOptOutSmartRouting, 
        stringUtf8 _orderClearingAccount,
        stringUtf8 _orderClearingIntent,
        intDec $ boolBinary _orderNotHeld
      ]
    , if isJust _conUnderComp then
        let 
          uc = fromJust _conUnderComp
        in 
          [ intDec $ _ucConId uc
          , buildDecimal $ _ucDelta uc
          , buildDecimal $ _ucPrice uc
          ]
      else
        [ intDec $ boolBinary False]
    , [ maybe bEmpty stringUtf8 _orderAlgoStrategy
      ]
    , if isJust _orderAlgoStrategy then
        intDec (Map.size _orderAlgoParams) : concatMap (\(k,v) -> map stringUtf8 [k,v]) (Map.toList _orderAlgoParams)
      else
        []
    , [ intDec $ boolBinary _orderWhatIf 
      ]
    ]

-- -----------------------------------------------------------------------------

createCancelOrderMsg :: Int -> Msg
createCancelOrderMsg orderid = return $ ibMsg 1 CancelOrderT [intDec orderid]

-- -----------------------------------------------------------------------------

createRequestOpenOrdersMsg :: Msg
createRequestOpenOrdersMsg = return $ ibMsg 1 ReqOpenOrdersT []

-- -----------------------------------------------------------------------------

createRequestAccountDataMsg :: Int -> Bool -> String -> Msg
createRequestAccountDataMsg sversion subscribe accountcode = 
  return $ ibMsg 2 ReqAccountDataT $ 
    intDec (boolBinary subscribe) : [stringUtf8 accountcode | sversion >= 9]

-- -----------------------------------------------------------------------------

createRequestExecutionsMsg :: Int -> IBExecutionFilter -> Msg
createRequestExecutionsMsg requestid IBExecutionFilter{..} =
  return $ ibMsg 3 ReqExecutionsT 
    [ intDec requestid
    , intDec (fromMaybe 0 _efClientId)
    , stringUtf8 (fromMaybe "" _efAcctCode)
    , stringUtf8 (maybe "" ibFormatTime _efFromDateTime)
    , stringUtf8 (fromMaybe "" _efSymbol)
    , stringUtf8 (maybe "" encode _efSecType)
    , stringUtf8 (maybe "" encode _efExchange)
    , stringUtf8 (maybe "" encode _efSide)
    ]

-- -----------------------------------------------------------------------------

createRequestIdsMsg :: Int -> Msg
createRequestIdsMsg numids = return $ ibMsg 1 ReqIdsT [intDec numids]

-- -----------------------------------------------------------------------------

createRequestContractDataMsg :: Int -> Int -> IBContract -> Msg
createRequestContractDataMsg sversion requestid IBContract{..} =
  return $ ibMsgConcat 7 ReqContractDataT
    [ [ intDec requestid
      , intDec _conId
      , stringUtf8 _conSymbol
      , bEncode _conSecType
      , stringUtf8 (maybe "" ibFormatDay _conExpiry)
      , buildDecimal _conStrike
      , stringUtf8 (maybe "" encode _conRight)
      , maybe bEmpty buildDecimal _conMultiplier
      , stringUtf8 (show _conExchange)
      , stringUtf8 (show _conCurrency)
      , stringUtf8 _conLocalSymbol
      ]
    , [ stringUtf8 _conTradingClass | sversion >= minServerVersionTradingClass
      ]
    , [ intDec $ boolBinary _conIncludeExpired
      , stringUtf8 (maybe "" encode _conSecIdType)
      , stringUtf8 _conSecId
      ]
    ]

-- -----------------------------------------------------------------------------

createRequestAutoOpenOrdersMsg :: Bool -> Msg
createRequestAutoOpenOrdersMsg autobind = 
  return $ ibMsg 1 ReqAutoOpenOrdersT [intDec $ boolBinary autobind]

-- -----------------------------------------------------------------------------

createRequestAllOpenOrdersMsg :: Msg
createRequestAllOpenOrdersMsg = return $ ibMsg 1 ReqAllOpenOrdersT []

-- -----------------------------------------------------------------------------

createRequestManagedAccountsMsg :: Msg
createRequestManagedAccountsMsg = return $ ibMsg 1 ReqManagedAccountsT []

-- -----------------------------------------------------------------------------

createRequestHistoricalDataMsg :: Int -> Int -> IBContract -> LocalTime -> IBDuration -> Int -> IBBarBasis -> Bool -> IBFormatDate -> Msg
createRequestHistoricalDataMsg sversion tickerid IBContract{..} enddatetime duration@(IBDuration i u) barsize barbasis userth formatdate 
  | i <= 0 = Left InvalidDuration
  | i > 1 && u == Y = Left InvalidDuration
  | barsize `notElem` [1,5,15,30,60,120,180,300,900,1800,3600,14400,86400] = Left InvalidBarSize
  | otherwise = return $ ibMsgConcat 5 ReqHistoricalDataT
      [ [ intDec tickerid
        ]
      , [ intDec _conId | sversion >= minServerVersionTradingClass]
      , [ stringUtf8 _conSymbol
        , bEncode _conSecType
        , stringUtf8 (maybe "" ibFormatDay _conExpiry)
        , buildDecimal _conStrike
        , stringUtf8 (maybe "" encode _conRight)
        , maybe bEmpty buildDecimal _conMultiplier
        , bEncode _conExchange
        , bEncode _conPrimaryExch
        , stringUtf8 (show _conCurrency)
        , stringUtf8 _conLocalSymbol
        ]
      , [ stringUtf8 _conTradingClass | sversion >= minServerVersionTradingClass]
      , [ intDec $ boolBinary _conIncludeExpired,
          stringUtf8 $ formatTime defaultTimeLocale "%Y%m%d %H:%M:%S" enddatetime, 
          stringUtf8 $ formatSeconds barsize, 
          bEncode duration,
          intDec $ boolBinary userth,
          bEncode barbasis,
          bEncode formatdate
        ]
      ]

-- -----------------------------------------------------------------------------

createCancelHistoricalDataMsg :: Int -> Msg
createCancelHistoricalDataMsg tickerid = 
  return $ ibMsg 1 CancelHistoricalDataT [intDec tickerid]

-- -----------------------------------------------------------------------------

createRequestCurrentTimeMsg :: Msg
createRequestCurrentTimeMsg = return $ ibMsg 1 ReqCurrentTimeT []

-- -----------------------------------------------------------------------------

createRequestRealTimeBarsMsg :: Int -> Int -> IBContract -> Int -> IBBarBasis -> Bool -> Msg
createRequestRealTimeBarsMsg sversion tickerid IBContract{..} barsize barbasis userth  
  | barsize /= 5 = Left InvalidBarSize
  | barbasis `notElem` [BarBasisTrades,BarBasisBid,BarBasisAsk,BarBasisMidpoint] = Left InvalidBarBasis
  | otherwise = return $ ibMsgConcat 2 ReqRealTimeBarsT
      [ [ intDec tickerid
        ]
      , [ intDec _conId | sversion >= minServerVersionTradingClass
        ]
      , [ stringUtf8 _conSymbol
        , bEncode _conSecType
        , stringUtf8 (maybe "" ibFormatDay _conExpiry)
        , buildDecimal _conStrike
        , stringUtf8 (maybe "" encode _conRight)
        , maybe bEmpty buildDecimal _conMultiplier
        , bEncode _conExchange
        , bEncode _conPrimaryExch
        , stringUtf8 (show _conCurrency)
        , stringUtf8 _conLocalSymbol
        ]
      , [ stringUtf8 _conTradingClass | sversion >= minServerVersionTradingClass
        ]
      , [ intDec barsize
        , bEncode barbasis
        , intDec $ boolBinary userth
        ]
      ]

-- -----------------------------------------------------------------------------

createCancelRealTimeBarsMsg :: Int -> Msg
createCancelRealTimeBarsMsg tickerid = return $ ibMsg 1 CancelRealTimeBarsT [intDec tickerid]

-- -----------------------------------------------------------------------------

createRequestGlobalCancelMsg :: Msg
createRequestGlobalCancelMsg = return $ ibMsg 1 ReqGlobalCancelT []

-- -----------------------------------------------------------------------------

createRequestMarketDataTypeMsg :: IBMarketDataType -> Msg
createRequestMarketDataTypeMsg marketdatatype = 
  return $ ibMsg 1 ReqMarketDataTypeT [bEncode marketdatatype]

-- -----------------------------------------------------------------------------

createRequestPositionsMsg :: Msg
createRequestPositionsMsg = return $ ibMsg 1 ReqPositionsT []

-- -----------------------------------------------------------------------------

createRequestAccountSummaryMsg :: Int -> Int -> IBGroup -> [IBTag] -> Msg
createRequestAccountSummaryMsg sversion reqid grp tags  
  | sversion < minServerVersionAccountSummary = Left InvalidServerVersion
  | otherwise = return $ ibMsg 1 ReqAccountSummaryT
      [ intDec reqid
      , bEncode grp
      , bSep ',' (map bEncode tags)
      ]

-- -----------------------------------------------------------------------------

createCancelAccountSummaryMsg :: Int -> Int -> Msg
createCancelAccountSummaryMsg sversion requestid 
  | sversion < minServerVersionAccountSummary = Left InvalidServerVersion
  | otherwise = return $ ibMsg 1 CancelAccountSummaryT [intDec requestid]

-- -----------------------------------------------------------------------------

createCancelPositionsMsg :: Int -> Msg
createCancelPositionsMsg sversion
  | sversion < minServerVersionAccountSummary = Left InvalidServerVersion
  | otherwise = return $ ibMsg 1 CancelPositionsT []




  










