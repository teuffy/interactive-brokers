module API.IB.Builder where

import Currency
import Data.Time 

import API.IB.Data
import API.IB.Enum

-- --------------------------------------------------------------------------
-- Contracts

future :: String -> String -> Maybe Day -> IBExchange -> Currency -> IBContract
future symbol_family symbol expiry_dt exchange currency = newIBContract 
  { _conSymbol = symbol_family
  , _conSecType = IBFuture
  , _conExpiry = expiry_dt
  , _conExchange = exchange
  , _conCurrency = currency
  , _conLocalSymbol = symbol
  , _conPrimaryExch = exchange
  }

-- --------------------------------------------------------------------------
-- Orders

marketOrder :: Int -> Int -> IBOrderAction -> Int -> IBOrder
marketOrder order_id client_id action quantity = newIBOrder
  { _orderId = order_id
  , _orderClientId = client_id
  , _orderAction = action
  , _orderTotalQuantity = quantity
  , _orderType = Market
  , _orderTransmit = True
  }

