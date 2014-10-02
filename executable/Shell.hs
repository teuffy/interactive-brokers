{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.Default
import           Data.Time
import           MVC                              hiding (loop,recv,send)
import           System.Console.Haskeline

import           API.IB

-----------------------------------------------------------------------------

conESZ4 :: IBContract
conESZ4 = future "ES" "ESZ4" (fromGregorian 2014 12 19) GLOBEX "USD" 

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
      Just "con" -> getContract >> go
      Just "price" -> getPrice >> go
      Just _ -> help >> go 

help :: InputT IB ()
help = outputStrLn "commands: help, quit, con, price"

quit :: InputT IB ()
quit = lift disconnect

getContract :: InputT IB ()
getContract = do
  void $ lift $ requestContractData conESZ4
  untilM $ do
    r <- lift recv
    case r of
      Just (IBResponse c@ContractData{}) -> outputStrLn (show c) >> return False
      Just (IBResponse ContractDataEnd{}) -> return True
      _ -> return False

getPrice :: InputT IB ()
getPrice = do
  void $ lift $ requestMarketData conESZ4 [] False
  untilRecv $ \case
    IBResponse tp@TickPrice{} -> outputStrLn (show tp) >> return True
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

