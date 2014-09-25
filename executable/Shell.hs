{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.Default
import           Data.Time
import           MVC                              hiding (loop)
import           MVC.Service
import           System.Console.Haskeline

import API.IB

-----------------------------------------------------------------------------

main :: IO ()
main = withIB def $ \(Service request response) -> do
  let 
    send' = liftIO . atomically . send request
    recv' = liftIO $ atomically $ recv response
    connect = execStateT connect' 0
    connect' = do
      r <- recv'
      case r of 
        Just (IBResponse Connection{..}) -> put _connServerVersion >> connect'
        Just (IBResponse NextValidId{}) -> return ()
        _ -> connect'
    getData = do
      sv <- lift get
      void $ send' $ IBRequest $ RequestContractData sv 1 conESZ4
      untilM $ do
        r <- lift recv'
        case r of
          Just (IBResponse c@ContractData{}) -> outputStrLn (show c) >> return False
          Just (IBResponse ContractDataEnd{}) -> return True
          _ -> return False
    loop = do
      minput <- getInputLine "IB> "
      case minput of
        Nothing -> return ()
        Just "" -> loop
        Just "quit" -> return ()
        Just "data" -> getData
        Just input -> outputStrLn ("Unrecognised command: " ++ input) >> loop
  sv <- connect
  void $ flip evalStateT sv $ runInputT defaultSettings loop

conESZ4 :: IBContract
conESZ4 = future "ES" "ESZ4" (fromGregorian 2014 12 19) GLOBEX "USD" 

untilM :: (Monad m) => InputT m Bool -> InputT m ()
untilM = loop
  where
  loop m = do
    r <- m
    unless r $ loop m



