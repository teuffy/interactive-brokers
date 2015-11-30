{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module API.IB.Enum where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Lazy.Builder     (Builder, stringUtf8)
import qualified Data.IntMap                      as IntMap (IntMap, Key,
                                                             fromList, lookup)
import           Data.Map                         (Map)
import qualified Data.Map                         as Map (fromList, lookup)
import           Data.Maybe
import           Data.Tuple

import           API.IB.Parse
import           API.IB.Util


-- -----------------------------------------------------------------------------

class ToIB a where
  encode :: a -> String
  
bEncode :: ToIB a => a -> Builder
bEncode = stringUtf8 . encode

-- -----------------------------------------------------------------------------

data IBRequestType = 
    ReqMktDataT                   -- DONE
  | CancelMktDataT                -- DONE
  | PlaceOrderT                   -- DONE
  | CancelOrderT                  -- DONE
  | ReqOpenOrdersT                -- DONE
  | ReqAccountDataT               -- DONE
  | ReqExecutionsT                -- DONE
  | ReqIdsT                       -- DONE
  | ReqContractDataT              -- DONE
  | ReqMktDepthT                  -- 2
  | CancelMktDepthT               -- 2
  | ReqNewsBulletinsT             -- 3
  | CancelNewsBulletinsT          -- 3
  | SetServerLogLevelT            -- 2
  | ReqAutoOpenOrdersT            -- DONE
  | ReqAllOpenOrdersT             -- DONE
  | ReqManagedAccountsT           -- DONE
  | ReqFAT                        -- TBC
  | ReplaceFAT                    -- TBC
  | ReqHistoricalDataT            -- DONE
  | ExerciseOptionsT              -- 3
  | ReqScannerSubscriptionT       -- 3
  | CancelScannerSubscriptionT    -- 3
  | ReqScannerParametersT         -- 3
  | CancelHistoricalDataT         -- DONE
  | ReqCurrentTimeT               -- DONE
  | ReqRealTimeBarsT              -- DONE
  | CancelRealTimeBarsT           -- DONE
  | ReqFundamentalDataT           -- 3
  | CancelFundamentalDataT        -- 3
  | ReqCalcImpliedVolT            -- 3
  | ReqCalcOptionPriceT           -- 3
  | CancelCalcImpliedVolT         -- 3
  | CancelCalcOptionPriceT        -- 3
  | ReqGlobalCancelT              -- DONE
  | ReqMarketDataTypeT            -- DONE
  | ReqPositionsT                 -- DONE
  | ReqAccountSummaryT            -- DONE
  | CancelAccountSummaryT         -- DONE
  | CancelPositionsT              -- DONE
    deriving (Eq,Ord,Read,Show)

instance Enum IBRequestType where
  fromEnum = fromJust . flip Map.lookup ibRequestTypesM
  toEnum = fromJust . flip IntMap.lookup ibRequestTypesM'

instance ToIB IBRequestType where
  encode = show . fromEnum

ibRequestTypesM :: Map IBRequestType Int
ibRequestTypesM = Map.fromList ibRequestTypesL

ibRequestTypesM' :: IntMap.IntMap IBRequestType
ibRequestTypesM' = IntMap.fromList $ map swap ibRequestTypesL

ibRequestTypesL :: [(IBRequestType, IntMap.Key)]
ibRequestTypesL = [
  (ReqMktDataT,1), 
  (CancelMktDataT,2), 
  (PlaceOrderT,3),
  (CancelOrderT,4),
  (ReqOpenOrdersT,5),
  (ReqAccountDataT,6),
  (ReqExecutionsT,7),
  (ReqIdsT,8),
  (ReqContractDataT,9),
  (ReqMktDepthT,10),
  (CancelMktDepthT,11),
  (ReqNewsBulletinsT,12),
  (CancelNewsBulletinsT,13),
  (SetServerLogLevelT,14),
  (ReqAutoOpenOrdersT,15),
  (ReqAllOpenOrdersT,16),
  (ReqManagedAccountsT,17),
  (ReqFAT,18),
  (ReplaceFAT,19),
  (ReqHistoricalDataT,20),
  (ExerciseOptionsT,21),
  (ReqScannerSubscriptionT,22),
  (CancelScannerSubscriptionT,23),
  (ReqScannerParametersT,24),
  (CancelHistoricalDataT,25),
  (ReqCurrentTimeT,49),
  (ReqRealTimeBarsT,50),
  (CancelRealTimeBarsT,51),
  (ReqFundamentalDataT,52),
  (CancelFundamentalDataT,53),
  (ReqCalcImpliedVolT,54),
  (ReqCalcOptionPriceT,55),
  (CancelCalcImpliedVolT,56),
  (CancelCalcOptionPriceT,57),
  (ReqGlobalCancelT,58),
  (ReqMarketDataTypeT,59),
  (ReqPositionsT,61),
  (ReqAccountSummaryT,62),
  (CancelAccountSummaryT,63),
  (CancelPositionsT,64)]

-- -----------------------------------------------------------------------------

data IBResponseType = 
    TickPriceT                    -- DONE
  | TickSizeT                     -- DONE
  | OrderStatusT                  -- DONE
  | ErrorMessageT                 -- DONE
  | OpenOrderT                    -- DONE
  | AccountValueT                 -- DONE
  | PortfolioValueT               -- DONE
  | AccountUpdateTimeT            -- DONE
  | NextValidIdT                  -- DONE
  | ContractDataT                 -- DONE
  | ExecutionDataT                -- DONE
  | MarketDepthT                  -- 2
  | MarketDepthL2T                -- 2
  | NewsBulletinsT                -- 3
  | ManagedAccountsT              -- DONE
  | ReceiveFAT                    -- TBC
  | HistoricalDataT               -- DONE
  | BondContractDataT             -- 3
  | ScannerParametersT            -- 3
  | ScannerDataT                  -- 3
  | TickOptionComputationT        -- TBC
  | TickGenericT                  -- DONE
  | TickStringT                   -- DONE
  | TickEFPT                      -- TBC
  | CurrentTimeT                  -- DONE
  | RealTimeBarT                  -- DONE
  | FundamentalDataT              -- 3
  | ContractDataEndT              -- DONE
  | OpenOrderEndT                 -- DONE
  | AccountDownloadEndT           -- 2
  | ExecutionDataEndT             -- DONE
  | DeltaNeutralValidationT       -- 3
  | TickSnapshotEndT              -- DONE
  | MarketDataTypeT               -- DONE
  | CommissionReportT             -- DONE
  | PositionT                     -- DONE
  | PositionEndT                  -- DONE
  | AccountSummaryT               -- DONE
  | AccountSummaryEndT            -- DONE
    deriving (Eq,Ord,Read,Show)

instance Enum IBResponseType where
  fromEnum = fromJust . flip Map.lookup ibResponseTypesM
  toEnum = fromJust . flip IntMap.lookup ibResponseTypesM'

ibResponseTypesM :: Map IBResponseType Int
ibResponseTypesM = Map.fromList ibResponseTypesL

ibResponseTypesM' :: IntMap.IntMap IBResponseType
ibResponseTypesM' = IntMap.fromList $ map swap ibResponseTypesL

ibResponseTypesL :: [(IBResponseType, IntMap.Key)]
ibResponseTypesL = [
  (TickPriceT, 1), 
  (TickSizeT, 2), 
  (OrderStatusT,3),
  (ErrorMessageT,4),
  (OpenOrderT,5),
  (AccountValueT,6),
  (PortfolioValueT,7),
  (AccountUpdateTimeT,8),
  (NextValidIdT,9),
  (ContractDataT,10),
  (ExecutionDataT,11),
  (MarketDepthT,12),
  (MarketDepthL2T,13),
  (NewsBulletinsT,14),
  (ManagedAccountsT,15),
  (ReceiveFAT,16),
  (HistoricalDataT,17),
  (BondContractDataT,18),
  (ScannerParametersT,19),
  (ScannerDataT,20),
  (TickOptionComputationT,21),
  (TickGenericT,45),
  (TickStringT,46),
  (TickEFPT,47),
  (CurrentTimeT,49),
  (RealTimeBarT,50),
  (FundamentalDataT,51),
  (ContractDataEndT,52),
  (OpenOrderEndT,53),
  (AccountDownloadEndT,54),
  (ExecutionDataEndT,55),
  (DeltaNeutralValidationT,56),
  (TickSnapshotEndT,57),
  (MarketDataTypeT,58),
  (CommissionReportT,59),
  (PositionT,61),
  (PositionEndT,62),
  (AccountSummaryT,63),
  (AccountSummaryEndT,64)]

-- -----------------------------------------------------------------------------

data IBTickType =
    BidSize
  | Bid
  | Ask
  | AskSize
  | Last
  | LastSize
  | High
  | Low
  | Volume
  | Close
  | BidOption
  | AskOption
  | LastOption
  | ModelOption
  | Open        
  | Low13Week 
  | High13Week
  | Low26Week 
  | High26Week
  | Low52Week 
  | High52Week
  | AvgVolume  
  | OpenInterest
  | OptionHistoricalvol
  | OptionImpliedVol
  | OptionBidExch
  | OptionAskExch
  | OptionCallOpenInterest
  | OptionPutOpenInterest
  | OptionCallVolume
  | OptionPutVolume
  | IndexFuturePremium
  | BidExch
  | AskExch
  | AuctionVolume
  | AuctionPrice
  | AuctionImbalance
  | MarkPrice
  | BidEFPComputation 
  | AskEFPComputation 
  | LastEFPComputation
  | OpenEFPComputation
  | HighEFPComputation
  | LowEFPComputation
  | CloseEFPComputation
  | LastTimestamp
  | Shortable
  | FundamentalRatios
  | RTVolume
  | Halted
  | BidYield
  | AskYield
  | LastYield
  | CustOptionComputation
  | TradeCount
  | TradeRate
  | VolumeRate
  | LastRTHTrade
  | RegulatoryDisturbance
  deriving (Eq,Ord,Enum,Show)

instance ToIB IBTickType where
  encode RegulatoryDisturbance = "61"
  encode x = show $ fromEnum x 

parseIBTickType :: Parser IBTickType
parseIBTickType = do
  v <- decimal :: Parser Int 
  if 
    | v >= 0 && v <= 57 -> return (toEnum v)
    | v == 61 -> return RegulatoryDisturbance
    | otherwise -> mzero

-- -----------------------------------------------------------------------------

data IBGenericTickType = 
    GenOptionVolume
  | GenOptionOpenInterest
  | GenHistoricalVolatility 
  | GenOptionImpliedVolatility 
  | GenIndexFuturePremium 
  | GenMiscellaneousStats 
  | GenMarkPrice 
  | GenAuctionValues 
  | GenRealTimeVolume 
  | GenShortable
  | GenInventory
  | GenFundamentalRatios
  | GenNews
  | GenRealTimeHistoricalVolatility
  | GenDividends
    deriving (Eq,Show)

instance ToIB IBGenericTickType where
  encode GenOptionVolume = "100"
  encode GenOptionOpenInterest = "101"
  encode GenHistoricalVolatility  = "104"
  encode GenOptionImpliedVolatility  = "106"
  encode GenIndexFuturePremium  = "162"
  encode GenMiscellaneousStats  = "165"
  encode GenMarkPrice  = "221"
  encode GenAuctionValues  = "225"
  encode GenRealTimeVolume  = "233"
  encode GenShortable = "236"
  encode GenInventory = "256"
  encode GenFundamentalRatios = "258"
  encode GenNews = "292"
  encode GenRealTimeHistoricalVolatility = "411"
  encode GenDividends = "456"

-- -----------------------------------------------------------------------------

data IBOrderStatus =
    PendingAPI  -- TODO: merge with PendingSubmit/PendingCancel?
  | CancelledAPI -- TODO: merge with Cancelled?
  | PendingSubmit
  | PendingCancel
  | PreSubmitted
  | Submitted
  | Cancelled
  | Filled 
  | Inactive
  deriving (Eq,Ord,Enum,Show,Read)

parseIBOrderStatus :: Parser IBOrderStatus
parseIBOrderStatus = 
  return PendingAPI <* string "ApiPending" <|>
  return CancelledAPI <* string "ApiCancelled" <|>
  return PendingSubmit <* string "PendingSubmit" <|>
  return PendingCancel <* string "PendingCancel" <|>
  return PreSubmitted <* string "PreSubmitted" <|>
  return Submitted <* string "Submitted" <|>
  return Cancelled <* string "Cancelled" <|>
  return Filled <* string "Filled" <|>
  return Inactive <* string "Inactive"

-- -----------------------------------------------------------------------------

data IBOrderWhyHeld =
    NoReason
  | Locate
  deriving (Eq,Ord,Enum,Show,Read)

parseIBOrderWhyHeld :: Parser IBOrderWhyHeld
parseIBOrderWhyHeld = 
  option NoReason (return Locate <* string "locate")

-- -----------------------------------------------------------------------------

data IBMarketDataType = 
    RealTime 
  | Frozen
    deriving (Eq,Ord,Enum,Show)

instance ToIB IBMarketDataType where
  encode RealTime = "1"
  encode Frozen = "2"

parseIBMarketDataType :: Parser IBMarketDataType
parseIBMarketDataType =
  return RealTime <* char '1' <|>
  return Frozen <* char '2'

-- -----------------------------------------------------------------------------

data IBFormatDate = 
    IBFDDateTime
  | IBFDSecs
    deriving (Eq,Ord,Read,Show)

instance ToIB IBFormatDate where 
  encode IBFDDateTime = "1"
  encode IBFDSecs = "2"

-- -----------------------------------------------------------------------------

data IBDurationUnit = 
    S 
  | D 
  | W 
  | M 
  | Y 
    deriving (Eq,Ord,Read,Show)

instance ToIB IBDurationUnit where
  encode = show

-- -----------------------------------------------------------------------------

data IBDuration = IBDuration Int IBDurationUnit

instance Show IBDuration where
  show (IBDuration i u) = show i ++ " " ++ show u

instance ToIB IBDuration where
  encode = show

-- -----------------------------------------------------------------------------

data IBGroup = 
    All
  | Group String
    deriving (Eq, Show)

instance ToIB IBGroup where
  encode All = "All"
  encode (Group grp) = grp

-- -----------------------------------------------------------------------------

data IBTag = 
    AccountCode
  | AccountReady
  | AccountType
  | AccruedCashC
  | AccruedCashS
  | AccruedCash
  | AccruedDividendC
  | AccruedDividendS
  | AccruedDividend
  | AvailableFundsC
  | AvailableFundsS
  | AvailableFunds
  | BillableC
  | BillableS
  | Billable
  | BuyingPower
  | CashBalance
  | CorporateBondValue
  | Currency
  | Cushion
  | DayTradesRemaining
  | DayTradesRemainingT1
  | DayTradesRemainingT2
  | DayTradesRemainingT3
  | DayTradesRemainingT4
  | EquityWithLoanValueC
  | EquityWithLoanValueS
  | EquityWithLoanValue
  | ExcessLiquidityC
  | ExcessLiquidityS
  | ExcessLiquidity
  | ExchangeRate
  | FullAvailableFundsC
  | FullAvailableFundsS
  | FullAvailableFunds
  | FullExcessLiquidityC
  | FullExcessLiquidityS
  | FullExcessLiquidity
  | FullInitMarginReqC
  | FullInitMarginReqS
  | FullInitMarginReq
  | FullMaintMarginReqC
  | FullMaintMarginReqS
  | FullMaintMarginReq
  | FundValue
  | FutureOptionValue
  | FuturesPNL
  | FxCashBalance
  | GrossPositionValueS
  | GrossPositionValue
  | HighestSeverity 
  | IndianStockHaircutC
  | IndianStockHaircutS
  | IndianStockHaircut
  | InitMarginReqC
  | InitMarginReqS
  | InitMarginReq
  | LeverageS
  | Leverage 
  | LookAheadAvailableFundsC
  | LookAheadAvailableFundsS
  | LookAheadAvailableFunds
  | LookAheadExcessLiquidityC
  | LookAheadExcessLiquidityS
  | LookAheadExcessLiquidity
  | LookAheadInitMarginReqC
  | LookAheadInitMarginReqS
  | LookAheadInitMarginReq
  | LookAheadMaintMarginReqC
  | LookAheadMaintMarginReqS
  | LookAheadMaintMarginReq
  | LookAheadNextChange
  | MaintMarginReqC
  | MaintMarginReqS
  | MaintMarginReq
  | MoneyMarketFundValue
  | MutualFundValue
  | NetDividend
  | NetLiquidationC
  | NetLiquidationS
  | NetLiquidation
  | NetLiquidationByCurrency
  | OptionMarketValue
  | PASharesValueC
  | PASharesValueS
  | PASharesValue
  | PostExpirationExcessC
  | PostExpirationExcessS
  | PostExpirationExcess
  | PostExpirationMarginC
  | PostExpirationMarginS
  | PostExpirationMargin
  | PreviousDayEquityWithLoanValueS
  | PreviousDayEquityWithLoanValue
  | RealizedPnL
  | RegTEquity
  | RegTEquityS
  | RegTMarginS
  | RegTMargin
  | SegmentTitleC
  | SegmentTitleS
  | SettledCash
  | SMAS
  | SMA
  | StockMarketValue
  | TBillValue
  | TBondValue
  | TotalCashBalance
  | TotalCashValueC
  | TotalCashValueS
  | TotalCashValue
  | TradingTypeS
  | UnrealizedPnL
  | WarrantValue
  | WhatIfPMEnabled
    deriving (Eq,Ord,Read,Show)

instance ToIB IBTag where
  encode = show

parseIBTag :: Parser IBTag
parseIBTag = do
  s <- stripChars "+-" <$> parseString
  case stringToEnum s of
    Just e -> return e
    Nothing -> fail ""

type IBTags = [IBTag]

-- -----------------------------------------------------------------------------

data IBBarBasis = 
    BarBasisTrades
  | BarBasisBid
  | BarBasisAsk
  | BarBasisMidpoint
  | BarBasisBidAsk
  | BarBasisHistoricalVolatility
  | BarBasisOptionImpliedVolatility
    deriving (Eq,Ord,Enum,Show)

instance ToIB IBBarBasis where
  encode BarBasisTrades = "TRADES"
  encode BarBasisBid = "BID"
  encode BarBasisAsk = "ASK"
  encode BarBasisMidpoint = "MIDPOINT"
  encode BarBasisBidAsk = "BID_ASK"
  encode BarBasisHistoricalVolatility = "HISTORICAL_VOLATILITY"
  encode BarBasisOptionImpliedVolatility = "OPTION_IMPLIED_VOLATILITY"

-- -----------------------------------------------------------------------------

data IBOrderAction = 
    Buy
  | Sell 
    deriving (Eq,Ord,Show,Enum)

instance ToIB IBOrderAction where
  encode Buy = "BUY"
  encode Sell = "SELL"

parseIBOrderAction :: Parser IBOrderAction
parseIBOrderAction = 
  return Buy <* string "BUY" <|>
  return Sell <* string "SELL"

-- -----------------------------------------------------------------------------

data IBOrderType =  
    Market 
  | Limit
  | Stop
  | StopLimit
    deriving (Eq,Ord,Show,Enum)

instance ToIB IBOrderType where
  encode Market = "MKT"
  encode Limit = "LMT"
  encode Stop = "STP"
  encode StopLimit = "STPLMT"

parseIBOrderType :: Parser IBOrderType
parseIBOrderType = 
  return Market <* string "MKT" <|>
  return Limit <* string "LMT"

-- -----------------------------------------------------------------------------

data IBOrderTimeInForce =  
    Day 
  | GoodTilCancelled
    deriving (Eq,Ord,Show,Enum) 

instance ToIB IBOrderTimeInForce where
  encode Day = "DAY"
  encode GoodTilCancelled = "GTC"

parseIBOrderTimeInForce :: Parser IBOrderTimeInForce
parseIBOrderTimeInForce = 
  return Day <* string "DAY" <|>
  return GoodTilCancelled <* string "GTC"

-- -----------------------------------------------------------------------------

data IBOrderOCAType = 
    CancelWithBlock
  | ReduceWithBlock 
  | ReduceNonBlock 
    deriving (Eq,Ord,Enum,Show)

instance ToIB IBOrderOCAType where
  encode CancelWithBlock = "1"
  encode ReduceWithBlock = "2"
  encode ReduceNonBlock = "3"

parseIBOrderOCAType :: Parser IBOrderOCAType
parseIBOrderOCAType = 
  return CancelWithBlock <* char '1' <|>
  return ReduceWithBlock <* char '2' <|>
  return ReduceNonBlock <* char '3'

-- -----------------------------------------------------------------------------

data IBOrderTriggerMethod = 
    TriggerDefault 
  | TriggerDoubleBidAsk
  | TriggerLast
  | TriggerDoubleLast
  | TriggerBidAsk
  | TriggerLastOrBidAsk
  | TriggerMidPoint
    deriving (Eq,Ord,Enum,Show)

instance ToIB IBOrderTriggerMethod where
  encode TriggerDefault = "0"
  encode TriggerDoubleBidAsk = "1"
  encode TriggerLast = "2"
  encode TriggerDoubleLast = "3"
  encode TriggerBidAsk = "4"
  encode TriggerLastOrBidAsk = "7"
  encode TriggerMidPoint = "8"

parseIBOrderTriggerMethod :: Parser IBOrderTriggerMethod
parseIBOrderTriggerMethod =
  return TriggerDefault <* char '0' <|>
  return TriggerDoubleBidAsk <* char '1' <|>
  return TriggerLast <* char '2' <|>
  return TriggerDoubleLast <* char '3' <|>
  return TriggerBidAsk <* char '4' <|>
  return TriggerLastOrBidAsk <* char '7' <|>
  return TriggerMidPoint <* char '8'

-- -----------------------------------------------------------------------------

data IBOrderRule80A = 
    Individual 
  | Agency
  | AgentOtherMember 
  | IndividualPTIA 
  | AgencyPTIA 
  | AgentOtherMemberPTIA 
  | IndividualPT 
  | AgencyPT 
  | AgentOtherMemberPT
    deriving (Eq,Ord,Enum,Show)

instance ToIB IBOrderRule80A where 
  encode Individual = "I"
  encode Agency = "A"
  encode AgentOtherMember = "W"
  encode IndividualPTIA = "J"
  encode AgencyPTIA = "U"
  encode AgentOtherMemberPTIA = "M"
  encode IndividualPT = "K"
  encode AgencyPT = "Y"
  encode AgentOtherMemberPT = "N"   

parseIBOrderRule80A :: Parser IBOrderRule80A
parseIBOrderRule80A =
  return Individual <* char 'I' <|>
  return Agency <* char 'A' <|>
  return AgentOtherMember <* char 'W' <|>
  return IndividualPTIA <* char 'J' <|>
  return AgencyPTIA <* char 'U' <|>
  return AgentOtherMemberPTIA <* char 'M' <|>
  return IndividualPT <* char 'K' <|>
  return AgencyPT <* char 'Y' <|>
  return AgentOtherMemberPT <* char 'N'

-- -----------------------------------------------------------------------------

data IBOrderOpenClose = 
    OrderOpen
  | OrderClose 
    deriving (Eq,Ord,Enum,Show)

instance ToIB IBOrderOpenClose where
  encode OrderOpen = "O"
  encode OrderClose = "C"

parseIBOrderOpenClose :: Parser IBOrderOpenClose
parseIBOrderOpenClose = 
  return OrderOpen <* char 'O' <|>
  return OrderClose <* char 'C'

-- -----------------------------------------------------------------------------

data IBOrderOrigin = 
    Customer 
  | Firm 
    deriving (Eq,Ord,Enum,Show)

instance ToIB IBOrderOrigin where 
  encode Customer = "0" 
  encode Firm = "1"

parseIBOrderOrigin :: Parser IBOrderOrigin
parseIBOrderOrigin = 
  return Customer <* char '0' <|>
  return Firm <* char '1'

-- -----------------------------------------------------------------------------

data IBOrderShortSaleSlot = 
    Held
  | Delivered 
    deriving (Eq,Ord,Enum,Show)

instance ToIB IBOrderShortSaleSlot where 
  encode Held = "1"
  encode Delivered = "2"

parseIBOrderShortSaleSlot :: Parser IBOrderShortSaleSlot
parseIBOrderShortSaleSlot = 
  return Held <* char '1' <|>
  return Delivered <* char '2'

-- -----------------------------------------------------------------------------

data IBOrderAuctionStrategy = 
    AuctionMatch 
  | AuctionImprovement 
  | AuctionTransparent
    deriving (Eq,Ord,Enum,Show)

instance ToIB IBOrderAuctionStrategy where 
  encode AuctionMatch = "1"
  encode AuctionImprovement = "2"
  encode AuctionTransparent = "3"

parseIBOrderAuctionStrategy :: Parser IBOrderAuctionStrategy
parseIBOrderAuctionStrategy = 
  return AuctionMatch <* char '1' <|>
  return AuctionImprovement <* char '2' <|>
  return AuctionTransparent <* char '3'

-- -----------------------------------------------------------------------------

data IBOrderVolatilityType = 
    Daily 
  | Annual 
    deriving (Eq,Ord,Enum,Show) 

instance ToIB IBOrderVolatilityType where 
  encode Daily = "1"
  encode Annual = "2"

parseIBOrderVolatilityType :: Parser IBOrderVolatilityType
parseIBOrderVolatilityType = 
  return Daily <* char '1' <|>
  return Annual <* char '2'

-- -----------------------------------------------------------------------------

data IBOrderRefPriceType = 
    BidAskMidpoint 
  | BidOrAsk
    deriving (Eq,Ord,Enum,Show) 

instance ToIB IBOrderRefPriceType where 
  encode BidAskMidpoint = "1"
  encode BidOrAsk = "2"

parseIBOrderRefPriceType :: Parser IBOrderRefPriceType
parseIBOrderRefPriceType = 
  return BidAskMidpoint <* char '1' <|>
  return BidOrAsk <* char '2'

-- -----------------------------------------------------------------------------

data IBOrderHedgeType = 
    Delta
  | Beta 
  | FX 
  | Pair 
    deriving (Eq,Ord,Enum,Show) 

instance ToIB IBOrderHedgeType where 
  encode Delta = "D" 
  encode Beta = "B" 
  encode FX = "F" 
  encode Pair = "P"

parseIBOrderHedgeType :: Parser IBOrderHedgeType
parseIBOrderHedgeType = 
  return Delta <* char 'D' <|>
  return Beta <* char 'B' <|>
  return FX <* char 'F' <|>
  return Pair <* char 'P'

-- -----------------------------------------------------------------------------

data IBExecutionSide =
    Bought
  | Sold
    deriving (Eq,Ord,Enum,Show) 

instance ToIB IBExecutionSide where
  encode Bought = "BOT"
  encode Sold = "SLD"

parseIBExecutionSide :: Parser IBExecutionSide
parseIBExecutionSide = 
  return Bought <* string "BOT" <|>
  return Sold <* string "SLD"

-- -----------------------------------------------------------------------------

data IBSecurityType =
    IBBag
  | IBForex
  | IBFuture
  | IBFutureOption
  | IBIndex
  | IBNews
  | IBOption
  | IBStock
    deriving (Eq,Ord,Enum,Show) 

instance ToIB IBSecurityType where
  encode IBBag = "BAG"
  encode IBForex = "CASH"
  encode IBFuture = "FUT"
  encode IBFutureOption = "FOP"
  encode IBIndex = "IND"
  encode IBNews = "NEWS"
  encode IBOption = "OPT"
  encode IBStock = "STK"
  
parseIBSecurityType :: Parser IBSecurityType
parseIBSecurityType =
  return IBBag <* string "BAG" <|>
  return IBForex <* string "CASH" <|>
  return IBFuture <* string "FUT" <|>
  return IBFutureOption <* "FOP" <|>
  return IBIndex <* "IND" <|>
  return IBNews <* "NEWS" <|>
  return IBOption <* string "OPT" <|>
  return IBStock <* string "STK"
  
-- -----------------------------------------------------------------------------

data IBExchange = 
    GLOBEX
  | LSE
  | NASDAQ
  | NYMEX
  | NYSE
  | DTB
  | ECBOT
  | Other String
    deriving (Eq,Ord,Read,Show)

instance ToIB IBExchange where
  encode (Other x) = x
  encode x = show x

parseIBExchange :: Parser IBExchange
parseIBExchange = do
  s <- parseString
  case reads s of
    [(x, "")] -> return x
    _ -> return (Other s)

-- -----------------------------------------------------------------------------

data IBRight = 
    Call 
  | Put
    deriving (Eq,Ord,Enum,Show) 

instance ToIB IBRight where
  encode Call = "C"
  encode Put = "P"

parseIBRight :: Parser IBRight
parseIBRight =
  return Call <* (string "C" <|> string "CALL") <|>
  return Put <* (string "P" <|> string "PUT")

-- -----------------------------------------------------------------------------

data IBSecurityIdType =
    CUSIP
  | ISIN
  | RIC
  | SEDOL
    deriving (Eq,Ord,Enum,Show)

instance ToIB IBSecurityIdType where
  encode = show

parseIBSecurityIdType :: Parser IBSecurityIdType
parseIBSecurityIdType =
  return CUSIP <* string "CUSIP" <|>
  return ISIN <* string "ISIN" <|>
  return RIC <* string "RIC" <|>
  return SEDOL <* string "SEDOL"

-- -----------------------------------------------------------------------------

data IBComboLegAction = 
    ComboLegBuy 
  | ComboLegSell
  | ComboLegSellShort 
  | ComboLegSellShortX
    deriving (Eq,Ord,Enum,Show)

instance ToIB IBComboLegAction where
  encode ComboLegBuy = "BUY"
  encode ComboLegSell = "SELL"
  encode ComboLegSellShort = "SSHORT"
  encode ComboLegSellShortX = "SSHORTX"

parseIBComboLegAction :: Parser IBComboLegAction
parseIBComboLegAction = 
  return ComboLegBuy <* string "BUY" <|>
  return ComboLegSell <* string "SELL" <|>
  return ComboLegSellShort <* string "SSHORT" <|>
  return ComboLegSellShortX <* string "SSHORTX"

-- -----------------------------------------------------------------------------

data IBComboLegShortSaleSlot =
    NotApplicable
  | ClearingBroker
  | ThirdParty
    deriving (Eq,Ord,Enum,Show)

instance ToIB IBComboLegShortSaleSlot where
  encode NotApplicable = "0"
  encode ClearingBroker = "1"
  encode ThirdParty = "2"

parseIBComboLegShortSaleSlot :: Parser IBComboLegShortSaleSlot
parseIBComboLegShortSaleSlot = 
  return NotApplicable <* string "0" <|>
  return ClearingBroker <* string "1" <|>
  return ThirdParty <* string "2"  

-- -----------------------------------------------------------------------------

data IBComboLegOpenClose =
    ComboLegParent
  | ComboLegOpen 
  | ComboLegClose
  | ComboLegUnknown
    deriving (Eq,Ord,Enum,Show)

instance ToIB IBComboLegOpenClose where
  encode ComboLegParent = "0"
  encode ComboLegOpen = "1"
  encode ComboLegClose = "2"
  encode ComboLegUnknown = "3"

parseIBComboLegOpenClose :: Parser IBComboLegOpenClose
parseIBComboLegOpenClose = 
  return ComboLegParent <* string "0" <|>
  return ComboLegOpen <* string "1" <|>
  return ComboLegClose <* string "2" <|>
  return ComboLegUnknown <* string "3"
