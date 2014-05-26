module Main where

import           Control.Lens
import           Data.ByteString.Char8 as B (pack)
import           MVC
import           MVC.Prelude
import           MVC.Service

import           MVC.Socket

-- -----------------------------------------------------------------------------

model :: Model () ServiceOut (Either ServiceIn String)
model = asPipe $ do
  yield $ Left $ ServiceCommand ServiceStart
  yield $ Left $ Send $ B.pack "hello\n"
  yield $ Left $ Send $ B.pack "yo\n"
  --yield $ Left $ Command ServiceStop
  for cat (yield . Right . show)

external :: Managed (View (Either ServiceIn String), Controller ServiceOut)
external = do
  (socketV,socketC) <- toManagedMVC $ toManagedService $ socketService $ SocketParams "127.0.0.1" "44444" False
  return (handles _Left socketV <> handles _Right stdoutLines,socketC)

main :: IO ()
main = runMVC () model external
