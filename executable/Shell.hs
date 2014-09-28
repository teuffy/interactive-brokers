{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.Default
import           Data.Time
import           MVC                              hiding (loop,recv,send)
import           System.Console.Haskeline

import           API.IB

-----------------------------------------------------------------------------

main :: IO ()
main = runIB def $ runInputT defaultSettings $ lift connect >> loop
  where
  loop = do
    minput <- getInputLine "IB> "
    case minput of
      Nothing -> return ()
      Just "" -> loop
      Just "quit" -> return ()
      Just "data" -> getData
      Just input -> outputStrLn ("Unrecognised command: " ++ input) >> loop    
  getData = do
    Just conn <- lift connection
    let sv = _connServerVersion conn
    void $ lift $ send $ IBRequest $ RequestContractData sv 1 conESZ4
    untilM $ do
      r <- lift recv
      case r of
        Just (IBResponse c@ContractData{}) -> outputStrLn (show c) >> return False
        Just (IBResponse ContractDataEnd{}) -> return True
        _ -> return False

conESZ4 :: IBContract
conESZ4 = future "ES" "ESZ4" (fromGregorian 2014 12 19) GLOBEX "USD" 

untilM :: (Monad m) => InputT m Bool -> InputT m ()
untilM = loop
  where
  loop m = do
    r <- m
    unless r $ loop m



