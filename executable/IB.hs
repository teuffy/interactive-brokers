{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent        (threadDelay)
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default
import           Data.Time
import           System.IO                 (stdout)
import           System.Environment
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Logger

import           API.IB


-- conES, conNQ :: IBContract
-- conES = future "ES" "ESM5" (fromGregorian 2015 06 19) GLOBEX "USD"
conES :: IBContract
conES = future "ES" "ESM5" (Just $ fromGregorian 2015 06 19) GLOBEX "USD" 

-----------------------------------------------------------------------------

main :: IO ()
main = do
  handler <- streamHandler stdout DEBUG >>= \h -> return $
    setFormatter h $ simpleLogFormatter "$time $loggername $prio: $msg"
  updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [handler])
  getArgs >>= cmd

-----------------------------------------------------------------------------

cmd :: [String] -> IO ()
cmd args = case args of
  "contracts" : sf : [] -> cmdContracts sf ""
  "contracts" : sf : s : [] -> cmdContracts sf s
  "history" : sf : [] -> cmdHistory sf ""
  "history" : sf : s : [] -> cmdHistory sf s
  "price" : sf : [] -> cmdPrice sf ""
  "price" : sf : s : [] -> cmdPrice sf s
  _ -> cmdUsage

-----------------------------------------------------------------------------

cmdContracts :: String -> String -> IO ()
cmdContracts sf s = ib $ do 
  connect
  reqContracts sf s >>= liftIO . print
  stop

cmdHistory :: String -> String -> IO ()
cmdHistory sf s = ib $ do 
  connect
  mcon <- reqContracts sf s
  case mcon of 
    Nothing -> return ()
    Just cd -> reqHistory1 (_cdSummary cd) >>= liftIO . print
  stop

cmdPrice :: String -> String -> IO ()
cmdPrice sf s = ib $ do 
  connect
  mcon <- reqContracts sf s
  case mcon of 
    Nothing -> return ()
    Just cd -> reqPrices (_cdSummary cd) >>= liftIO . print
  stop

{-

cmdPosition :: String -> String -> IO ()
cmdPosition sf s = ib $ do 
  connect
  mcon <- reqContracts sf s
  case mcon of 
    Nothing -> return ()
    Just cd -> reqPositions (_cdSummary cd) >>= liftIO . print
  stop

-}
{-
cmdHistory1 :: String -> String -> IO (Maybe [IBHistoricalDataItem])
cmdHistory1 sf s = ib $ do 
  connect
  mcon <- reqContracts sf s
  {-
  mcon >>= \case 
    Nothing -> do 
        liftIO $ putStrLn "Failed"
        return Nothing
    Just cd -> reqHistory (_cdSummary cd)

  -}      
  case mcon of 
             Nothing -> return ()
             Just cd -> reqHistory (_cdSummary cd)
  --kk >>= liftIO . print
  --return (kk)
  
  stop
  --return ()
-}
-----------------------------------------------------------------------------

req :: IB (Maybe Int) -> (Int -> IB (Maybe a)) -> IB (Maybe a)
req r p = r >>= \case 
  Nothing -> do
    liftIO $ putStrLn "Request failed"
    return Nothing
  Just rid -> p rid

reqContracts :: String -> String -> IB (Maybe IBContractDetails)
reqContracts sf s = req (requestContractData (ibContract sf s)) $ \rid -> untilRecv $ \case
  IBResponse (ContractData rid c) -> if null s || s == _conLocalSymbol (_cdSummary c)
    then return (AResult c)
    else return AContinue
  IBResponse (ContractDataEnd rid) -> return AStop
  _ -> return AContinue

reqHistory :: IBContract -> IB (Maybe [IBHistoricalDataItem])
reqHistory con = do
  refDate <- liftIO today
  req (requestHistoricalData con refDate (IBDuration 1 D) 3600 BarBasisTrades False IBFDDateTime) $ \rid -> untilRecv $ \case
    IBResponse (HistoricalData rid hd) -> return (AResult hd)
    _ -> return AContinue   

-- important IBDuration = period to mesure in S D W etc.. Int after in the grouping size ex 60 S) = 1 mn and bar is set to 5s
reqHistory1 :: IBContract -> IB (Maybe [IBHistoricalDataItem])
reqHistory1 con = do
  refDate <- liftIO today
  req (requestHistoricalData con refDate (IBDuration 1 D) 14400 BarBasisTrades False IBFDDateTime) $ \rid -> untilRecv $ \case
    IBResponse (HistoricalData rid hd) -> return (AResult hd)
    _ -> return AContinue       

reqPrices :: IBContract -> IB (Maybe Decimal)
reqPrices con = do
  --refDate <- liftIO today
  req (requestMarketData con [] False) $ \rid -> untilRecv $ \case
    --IBResponse (HistoricalData rid hd) -> return (AResult hd)
    -- IBResponse (TickPrice rid _ tp _ )-> return (AResult tp) 
     IBResponse (TickPrice rid tt p c) -> return (AResult p) 
     _ -> return AContinue   

{-
reqPositions :: IBContract -> IB (Maybe Int)
reqPositions act = do
  --refDate <- liftIO today
  req (requestPositions) $ \rid -> untilRecv $ \case
    --IBResponse (HistoricalData rid hd) -> return (AResult hd)
    -- IBResponse (TickPrice rid _ tp _ )-> return (AResult tp) 
     IBResponse (Position) -> return (AResult p) 
     _ -> return AContinue   


-}

cmdUsage :: IO ()
cmdUsage = putStrLn "ib [hello | contracts SYMBOL-FAMILY [SYMBOL] | history SYMBOL-FAMILY [SYMBOL] | price SYMBOL-FAMILY [SYMBOL]"

--ibContract :: String -> String -> IBContract
--ibContract sf s = future sf s Nothing GLOBEX "USD"
--ibContract sf s = future sf s Nothing NYMEX "USD" 

ibContract :: String -> String -> IBContract
--ibContract sf s = future sf s Nothing GLOBEX "USD"
ibContract sf s 
  | elem sf ["CL","NG"] = future sf s Nothing NYMEX "USD" 
  | elem sf ["ES","NQ"] = future sf s Nothing GLOBEX "USD"
  | otherwise = future sf s Nothing GLOBEX "USD" 

ib :: IB () -> IO ()
ib cmds = finally (runIB def cmds) (threadDelay 1000000)

-----------------------------------------------------------------------------

data Action a = AStop | AContinue | AResult a

untilRecv :: (ServiceOut -> IB (Action a)) -> IB (Maybe a)
untilRecv check = untilM $ recv >>= \case
  Nothing -> return AStop
  Just r -> liftIO (print r) >> check r -- TODO: remove print after debugging

untilM :: (Monad m) => m (Action a) -> m (Maybe a)
untilM = go
  where
  go m = m >>= \case
    AStop -> return Nothing
    AContinue -> go m
    AResult x -> return (Just x)

-----------------------------------------------------------------------------

today :: IO LocalTime
today = do
  t <- getCurrentTime
  return $ LocalTime (utctDay t) midnight



