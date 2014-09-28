{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad
import           Data.Default
import           Data.Time
import           MVC
import           MVC.Service

import           API.IB        hiding (recv,send)

-- -----------------------------------------------------------------------------
-- Reference data

conESZ4 :: IBContract
conESZ4 = future "ES" "ESZ4" (fromGregorian 2014 12 19) GLOBEX "USD" 

-- -----------------------------------------------------------------------------
-- Main

main :: IO ()
main = withIB def $ \(Service request response) -> do
  let 
    send' = atomically . send request
    recv' = atomically $ recv response
    loop' sv done = do
      r <- recv'
      print r
      case r of 
        Just (IBResponse (Connection IBConnection{..})) -> 
          loop' _connServerVersion done
        Just (IBResponse NextValidId{}) -> 
          unless done $ do
            void $ send' $ IBRequest $ RequestMarketData sv 1 conESZ4 [] False
            loop' sv True
        _ -> loop' sv done
  loop' 0 False

