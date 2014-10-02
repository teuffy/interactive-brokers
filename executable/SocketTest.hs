{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.Default
import           MVC
import           MVC.Prelude
import           MVC.Service

import           MVC.Socket

-----------------------------------------------------------------------------

model :: Model () ServiceOut (Either ServiceIn String)
model = asPipe $ do
  
  yield $ Left $ ServiceCommand ServiceReportStatus
  
  yield $ Left $ ConnectionCommand $ Connect 0
  
  onCondition (== ServiceStatus ServiceActive) $
    yield $ Left $ Send "hello1\n"
  
  onCondition (== Stream "server: hello1\n") $
    yield $ Left $ ConnectionCommand Disconnect
  
  onCondition (== ServiceStatus ServicePending) $
    yield $ Left $ ConnectionCommand $ Connect 0
  
  onCondition (== ServiceStatus ServiceActive) $
    yield $ Left $ Send "hello2\n"
  
  for cat logEvent
  
  where
  
  logEvent = yield . Right . show

  onCondition cond action = go
    where
    go = do
      e <- await
      logEvent e
      if cond e
        then action
        else go

-----------------------------------------------------------------------------

external :: Managed (View (Either ServiceIn String), Controller ServiceOut)
external = do
  (socketV,socketC) <- toManagedMVC $ toManagedService $ socketService $ 
    def
      & scDebug .~ True
      & scSocketParams .~ SocketParams "127.0.0.1" "44444"
  return (handles _Left socketV <> handles _Right stdoutLines,socketC)

-----------------------------------------------------------------------------

main :: IO ()
main = runMVC () model external
