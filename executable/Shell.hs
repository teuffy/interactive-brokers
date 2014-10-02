{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad
import           Data.Default
import           Data.Time
import           MVC                              hiding (loop,recv,send)
import           System.Console.Haskeline

import           API.IB

-----------------------------------------------------------------------------

conES :: IBContract
conES = future "ES" "ESZ4" (fromGregorian 2014 12 19) GLOBEX "USD" 

conGC :: IBContract
conGC = future "GC" "GCZ4" (fromGregorian 2014 12 29) NYMEX "USD" 

-----------------------------------------------------------------------------

ibShell :: InputT IB ()
ibShell = lift connect >> go
  where
  go = do
    minput <- getInputLine "IB> "
    case minput of
      Nothing -> return ()
      Just "" -> go
      Just "help" -> help >> go
      Just "quit" -> quit
      Just "con es" -> getContract conES >> go
      Just "con gc" -> getContract conGC >> go
      Just "price es" -> getPrice conES >> go
      Just "price gc" -> getPrice conGC >> go
      Just _ -> help >> go 

help :: InputT IB ()
help = outputStrLn "commands: help, quit, con [es|gc], price [es|gc]"

quit :: InputT IB ()
quit = lift disconnect

getContract :: IBContract -> InputT IB ()
getContract con = lift (requestContractData con) >>= \case
  Nothing -> outputStrLn "Request failed"
  Just rid -> untilRecv $ \case
    IBResponse c@ContractData{..} -> if _cdReqId == rid 
      then outputStrLn (show c) >> return True
      else return False 
    _ -> return False

getPrice :: IBContract -> InputT IB ()
getPrice con = lift (requestMarketData con [] False) >>= \case
  Nothing -> outputStrLn "Request failed"
  Just rid -> untilRecv $ \case
    IBResponse tp@TickPrice{..} -> if _tpTickerId == rid 
      then outputStrLn (show tp) >> return True
      else return False
    _ -> return False

-----------------------------------------------------------------------------

untilRecv :: (ServiceOut -> InputT IB Bool) -> InputT IB ()
untilRecv check = untilM $ lift recv >>= \case
  Nothing -> return False
  Just r -> check r

untilM :: (Monad m) => InputT m Bool -> InputT m ()
untilM = go
  where
  go m = do
    r <- m
    unless r $ go m

-----------------------------------------------------------------------------

main :: IO ()
main = runIB def $ runInputT defaultSettings ibShell

