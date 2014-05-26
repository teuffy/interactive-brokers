{-# LANGUAGE OverloadedStrings,RecordWildCards #-}

module API.IB.Request where

import           Data.ByteString.Char8              (ByteString)
import qualified Data.ByteString.Char8              as BC
import           Data.ByteString.Lazy.Builder       (Builder, byteString,
                                                     stringUtf8)
import           Data.ByteString.Lazy.Builder.ASCII (doubleDec, intDec)
import qualified Data.Map                           as Map (size, toList)
import           Data.Maybe
import           Data.Monoid                        hiding (All, Product)
import           Data.String
import           Data.Time
import           Prelude                            hiding (takeWhile)
import           System.Locale

import           API.IB.Constant
import           API.IB.Data
import           API.IB.Enum
import           API.IB.Util

-- -----------------------------------------------------------------------------
-- Utilities

ibMsg :: Int -> IBRequestType -> [Builder] -> ByteString
ibMsg version reqtype builders = bMake sepC $ bEncode reqtype : intDec version : builders

ibMsgConcat :: Int -> IBRequestType -> [[Builder]] -> ByteString
ibMsgConcat version reqtype builders = ibMsg version reqtype $ concat builders   
  
-- -----------------------------------------------------------------------------
-- Message Factory

createClientIdMsg :: Int -> ByteString
createClientIdMsg clientid = BC.pack (show clientid) <> sepB

createClientVersionMsg :: Int -> ByteString
createClientVersionMsg cversion = BC.pack (show cversion) <> sepB

createMsg :: IBRequest -> Maybe ByteString
createMsg request = case request of 
  (RequestMarketData sv tid c gtl ss) -> createRequestMarketDataMsg sv tid c gtl ss
  (CancelMarketData sv tid) -> createCancelMarketDataMsg sv tid
  (PlaceOrder sv oid c o) -> createPlaceOrderMsg sv oid c o
  (CancelOrder sv oid) -> createCancelOrderMsg sv oid 
  (RequestOpenOrders sv) -> createRequestOpenOrdersMsg sv
  (RequestAccountData sv sub ac) -> createRequestAccountDataMsg sv sub ac
  (RequestExecutions sv rid ef) -> createRequestExecutionsMsg sv rid ef 
  (RequestIds sv n) -> createRequestIdsMsg sv n
  (RequestContractData sv rid c) -> createRequestContractDataMsg sv rid c
  (RequestAutoOpenOrders sv ab) -> createRequestAutoOpenOrdersMsg sv ab
  (RequestAllOpenOrders sv) -> createRequestAllOpenOrdersMsg sv
  (RequestManagedAccounts sv) -> createRequestManagedAccountsMsg sv
  (RequestHistoricalData sv tid c dt dur bs bb rth fd) -> createRequestHistoricalDataMsg sv tid c dt dur bs bb rth fd
  (CancelHistoricalData sv tid) -> createCancelHistoricalDataMsg sv tid
  (RequestCurrentTime sv) -> createRequestCurrentTimeMsg sv
  (RequestRealTimeBars sv tid c bs bb rth) -> createRequestRealTimeBarsMsg sv tid c bs bb rth
  (CancelRealTimeBars sv tid) -> createCancelRealTimeBarsMsg sv tid
  (RequestGlobalCancel sv) -> createRequestGlobalCancelMsg sv
  (RequestMarketDataType sv mdt) -> createRequestMarketDataTypeMsg sv mdt
  (RequestPositions sv) -> createRequestPositionsMsg sv
  (RequestAccountSummary sv rid g tl) -> createRequestAccountSummaryMsg sv rid g tl
  (CancelAccountSummary sv rid) -> createCancelAccountSummaryMsg sv rid
  (CancelPositions sv) -> createCancelPositionsMsg sv

-- -----------------------------------------------------------------------------

createRequestMarketDataMsg :: Int -> Int -> IBContract -> [IBGenericTickType] -> Bool -> Maybe ByteString
createRequestMarketDataMsg sversion tickerid IBContract{..} genticktypes snapshot = 
  return $ ibMsgConcat 10 ReqMktDataT
    [ [ intDec tickerid
      , intDec _conId
      , stringUtf8 _conSymbol
      , bEncode _conSecType
      , stringUtf8 _conExpiry
      , doubleDec _conStrike
      , stringUtf8 _conRight
      , stringUtf8 _conMultiplier
      , stringUtf8 _conExchange
      , stringUtf8 _conPrimaryExch
      , stringUtf8 _conCurrency
      , stringUtf8 _conLocalSymbol
      ]
    , [ stringUtf8 _conTradingClass | sversion >= minServerVersionTradingClass]
    , [ stringUtf8 "0"
      , bSep ',' (map bEncode genticktypes) 
      , intDec $ boolBinary snapshot
      ]
    ]

-- -----------------------------------------------------------------------------

createCancelMarketDataMsg :: Int -> Int -> Maybe ByteString
createCancelMarketDataMsg _ tickerid = return $ ibMsg 1 CancelMktDataT [intDec tickerid]

-- -----------------------------------------------------------------------------

createPlaceOrderMsg :: Int -> ByteString -> IBContract -> IBOrder -> Maybe ByteString
createPlaceOrderMsg sversion orderid IBContract{..} IBOrder{..} =
  return $ ibMsgConcat 40 PlaceOrderT
    [ [ byteString orderid
      , intDec _conId
      , stringUtf8 _conSymbol
      , bEncode _conSecType
      , stringUtf8 _conExpiry
      , doubleDec _conStrike
      , stringUtf8 _conRight
      , stringUtf8 _conMultiplier
      , stringUtf8 _conExchange
      , stringUtf8 _conPrimaryExch
      , stringUtf8 _conCurrency
      , stringUtf8 _conLocalSymbol
      ]
    , [ stringUtf8 _conTradingClass | sversion >= minServerVersionTradingClass
      ]
    , [ stringUtf8 _conSecIdType
      , stringUtf8 _conSecId
      ]
    , [ bEncode _orderAction
      , intDec _orderTotalQuantity
      , bEncode _orderType
      , maybe bEmpty doubleDec _orderLmtPrice
      , maybe bEmpty doubleDec _orderAuxPrice
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
                , stringUtf8 $ _comAction l
                , stringUtf8 $ _comExchange l
                , intDec $ _comOpenClose l
                , intDec $ _comShortSaleSlot l
                , stringUtf8 $ _comDesignatedLocation l
                , intDec $ _comExemptCode l
                ])
              _conComboLegs 
          else
            [intDec 0]
          <>
          if _orderComboLegsCount > 0 then
            intDec _orderComboLegsCount :
            map
              (doubleDec . _oclPrice)
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
      , doubleDec _orderDiscretionaryAmt
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
      , maybe bEmpty doubleDec _orderPercentOffset
      , intDec $ boolBinary _orderETradeOnly
      , intDec $ boolBinary _orderFirmQuoteOnly
      , maybe bEmpty doubleDec _orderNBBOPriceCap
      , maybe bEmpty bEncode _orderAuctionStrategy
      , maybe bEmpty doubleDec _orderStartingPrice
      , maybe bEmpty doubleDec _orderStockRefPrice
      , maybe bEmpty doubleDec _orderDelta
      , maybe bEmpty doubleDec _orderStockRangeLower
      , maybe bEmpty doubleDec _orderStockRangeUpper
      , intDec $ boolBinary _orderOverridePercentageConstraints
      , maybe bEmpty doubleDec _orderVolatility
      , maybe bEmpty bEncode _orderVolatilityType
      , maybe bEmpty stringUtf8 _orderDeltaNeutralOrderType
      , maybe bEmpty doubleDec _orderDeltaNeutralAuxPrice
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
      , maybe bEmpty doubleDec _orderTrailStopPrice
      , maybe bEmpty doubleDec _orderTrailingPercent
      , maybe bEmpty intDec _orderScaleInitLevelSize
      , maybe bEmpty intDec _orderScaleSubsLevelSize
      , maybe bEmpty doubleDec _orderScalePriceIncrement
      ]
    , if isJust _orderScalePriceIncrement && (fromJust _orderScalePriceIncrement > 0) then
        [ maybe bEmpty doubleDec _orderScalePriceAdjustValue
        , maybe bEmpty intDec _orderScalePriceAdjustInterval
        , maybe bEmpty doubleDec _orderScaleProfitOffset
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
          , doubleDec $ _ucDelta uc
          , doubleDec $ _ucPrice uc
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

createCancelOrderMsg :: Int -> ByteString -> Maybe ByteString
createCancelOrderMsg _ orderid = return $ ibMsg 1 CancelOrderT [byteString orderid]

-- -----------------------------------------------------------------------------

createRequestOpenOrdersMsg :: Int -> Maybe ByteString
createRequestOpenOrdersMsg _ = return $ ibMsg 1 ReqOpenOrdersT []

-- -----------------------------------------------------------------------------

createRequestAccountDataMsg :: Int -> Bool -> String -> Maybe ByteString
createRequestAccountDataMsg sversion subscribe accountcode = 
  return $ ibMsg 2 ReqAccountDataT $ 
    intDec (boolBinary subscribe) : [stringUtf8 accountcode | sversion >= 9]

-- -----------------------------------------------------------------------------

createRequestExecutionsMsg :: Int -> Int -> IBExecutionFilter -> Maybe ByteString
createRequestExecutionsMsg _ requestid IBExecutionFilter{..} =
  return $ ibMsg 3 ReqExecutionsT 
    [ intDec requestid
    , intDec _efClientId
    , stringUtf8 _efAcctCode
    , stringUtf8 _efTime
    , stringUtf8 _efSymbol
    , bEncode _efSecType
    , stringUtf8 _efExchange
    , stringUtf8 _efSide
    ]

-- -----------------------------------------------------------------------------

createRequestIdsMsg :: Int -> Int -> Maybe ByteString
createRequestIdsMsg _ numids = return $ ibMsg 1 ReqIdsT [intDec numids]

-- -----------------------------------------------------------------------------

createRequestContractDataMsg :: Int -> Int -> IBContract -> Maybe ByteString
createRequestContractDataMsg sversion requestid IBContract{..} =
  return $ ibMsgConcat 7 ReqContractDataT
    [ [ intDec requestid
      , intDec _conId
      , stringUtf8 _conSymbol
      , bEncode _conSecType
      , stringUtf8 _conExpiry
      , doubleDec _conStrike
      , stringUtf8 _conRight
      , stringUtf8 _conMultiplier
      , stringUtf8 _conExchange
      , stringUtf8 _conCurrency
      , stringUtf8 _conLocalSymbol
      ]
    , [ stringUtf8 _conTradingClass | sversion >= minServerVersionTradingClass
      ]
    , [ intDec $ boolBinary _conIncludeExpired
      , stringUtf8 _conSecIdType
      , stringUtf8 _conSecId
      ]
    ]

-- -----------------------------------------------------------------------------

createRequestAutoOpenOrdersMsg :: Int -> Bool -> Maybe ByteString
createRequestAutoOpenOrdersMsg _ autobind = 
  return $ ibMsg 1 ReqAutoOpenOrdersT [intDec $ boolBinary autobind]

-- -----------------------------------------------------------------------------

createRequestAllOpenOrdersMsg :: Int -> Maybe ByteString
createRequestAllOpenOrdersMsg _ = return $ ibMsg 1 ReqAllOpenOrdersT []

-- -----------------------------------------------------------------------------

createRequestManagedAccountsMsg :: Int -> Maybe ByteString
createRequestManagedAccountsMsg _ = return $ ibMsg 1 ReqManagedAccountsT []

-- -----------------------------------------------------------------------------

createRequestHistoricalDataMsg :: Int -> Int -> IBContract -> UTCTime -> IBDuration -> Int -> IBBarBasis -> Bool -> IBFormatDate -> Maybe ByteString
createRequestHistoricalDataMsg sversion tickerid IBContract{..} enddatetime duration@(IBDuration i u) barsize barbasis userth formatdate 
  | i <= 0 = Nothing
  | i > 1 && u == Y = Nothing
  | barsize `notElem` [1,5,15,30,60,120,180,300,900,1800,3600,86400] = Nothing 
  | otherwise = return $ ibMsgConcat 5 ReqHistoricalDataT
      [ [ intDec tickerid
        ]
      , [ intDec _conId | sversion >= minServerVersionTradingClass]
      , [ stringUtf8 _conSymbol
        , bEncode _conSecType
        , stringUtf8 _conExpiry
        , doubleDec _conStrike
        , stringUtf8 _conRight
        , stringUtf8 _conMultiplier
        , stringUtf8 _conExchange
        , stringUtf8 _conPrimaryExch
        , stringUtf8 _conCurrency
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

createCancelHistoricalDataMsg :: Int -> Int -> Maybe ByteString
createCancelHistoricalDataMsg _ tickerid = 
  return $ ibMsg 1  CancelHistoricalDataT [intDec tickerid]

-- -----------------------------------------------------------------------------

createRequestCurrentTimeMsg :: Int -> Maybe ByteString
createRequestCurrentTimeMsg _ = return $ ibMsg 1 ReqCurrentTimeT []

-- -----------------------------------------------------------------------------

createRequestRealTimeBarsMsg :: Int -> Int -> IBContract -> Int -> IBBarBasis -> Bool -> Maybe ByteString
createRequestRealTimeBarsMsg sversion tickerid IBContract{..} barsize barbasis userth  
  | barsize /= 5 = Nothing
  | barbasis `notElem` [BarBasisTrades,BarBasisBid,BarBasisAsk,BarBasisMidpoint] = Nothing 
  | otherwise = return $ ibMsgConcat 2 ReqRealTimeBarsT
      [ [ intDec tickerid
        ]
      , [ intDec _conId | sversion >= minServerVersionTradingClass
        ]
      , [ stringUtf8 _conSymbol
        , bEncode _conSecType
        , stringUtf8 _conExpiry
        , doubleDec _conStrike
        , stringUtf8 _conRight
        , stringUtf8 _conMultiplier
        , stringUtf8 _conExchange
        , stringUtf8 _conPrimaryExch
        , stringUtf8 _conCurrency
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

createCancelRealTimeBarsMsg :: Int -> Int -> Maybe ByteString
createCancelRealTimeBarsMsg _ tickerid = return $ ibMsg 1 CancelRealTimeBarsT [intDec tickerid]

-- -----------------------------------------------------------------------------

createRequestGlobalCancelMsg :: Int -> Maybe ByteString
createRequestGlobalCancelMsg _ = return $ ibMsg 1 ReqGlobalCancelT []

-- -----------------------------------------------------------------------------

createRequestMarketDataTypeMsg :: Int -> IBMarketDataType -> Maybe ByteString
createRequestMarketDataTypeMsg _ marketdatatype = 
  return $ ibMsg 1 ReqMarketDataTypeT [bEncode marketdatatype]

-- -----------------------------------------------------------------------------

createRequestPositionsMsg :: Int -> Maybe ByteString
createRequestPositionsMsg _ = return $ ibMsg 1 ReqPositionsT []

-- -----------------------------------------------------------------------------

createRequestAccountSummaryMsg :: Int -> Int -> IBGroup -> [IBTag] -> Maybe ByteString
createRequestAccountSummaryMsg sversion reqid grp tags  
  | sversion < minServerVersionAccountSummary = Nothing
  | otherwise = return $ ibMsg 1 ReqAccountSummaryT
      [ intDec reqid
      , bEncode grp
      , bSep ',' (map bEncode tags)
      ]

-- -----------------------------------------------------------------------------

createCancelAccountSummaryMsg :: Int -> Int -> Maybe ByteString
createCancelAccountSummaryMsg sversion requestid 
  | sversion < minServerVersionAccountSummary = Nothing
  | otherwise = return $ ibMsg 1 CancelAccountSummaryT [intDec requestid]

-- -----------------------------------------------------------------------------

createCancelPositionsMsg :: Int -> Maybe ByteString
createCancelPositionsMsg sversion 
  | sversion < minServerVersionAccountSummary = Nothing
  | otherwise = return $ ibMsg 1 CancelPositionsT []




  










