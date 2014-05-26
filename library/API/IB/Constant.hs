module API.IB.Constant where

import           Data.ByteString.Char8 (ByteString, singleton)
import           Data.Map              (Map)
import qualified Data.Map              as Map (fromList)

-- -----------------------------------------------------------------------------
-- Constants

clientVersion :: Int
clientVersion = 62

minServerVersionAccountSummary :: Int
minServerVersionAccountSummary = 67

minServerVersionTradingClass :: Int
minServerVersionTradingClass = 68

minServerVersionDeltaNeutralOpenClose :: Int
minServerVersionDeltaNeutralOpenClose = 66

sepC :: Char
sepC = '\NUL'

sepB :: ByteString
sepB = singleton sepC

ibTimeZoneMap :: Map String String
ibTimeZoneMap = Map.fromList
  [ ("Eastern Standard Time (Victoria)","Australia/Melbourne")
  , ("Eastern Standard Time (Queensland)","Australia/Brisbane")
  ]