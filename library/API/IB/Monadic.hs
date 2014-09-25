{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module API.IB.Monadic where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader             (MonadReader (..))
import           Control.Monad.State              (MonadState (..))
import           Control.Monad.Trans.Reader       (ReaderT)
import qualified Control.Monad.Trans.Reader       as R
import           Control.Monad.Trans.State.Strict (State, StateT)
import qualified Control.Monad.Trans.State.Strict as S
import           Data.Default
import           MVC                              hiding (loop)
import           MVC.Service

import API.IB.Connection
import API.IB.Data
import API.IB.Enum

-----------------------------------------------------------------------------

newtype IB r = 
  IB { unIB :: ReaderT IBService (StateT IBState IO) r}
  deriving (Functor,Applicative,Monad,MonadIO,MonadReader IBService,MonadState IBState)

runIB :: IBConfiguration -> IB a -> IO a
runIB cfg ib = withIB def $ \ibs -> 
  flip S.evalStateT (def & ibsConfig .~ (cfg & cfgAutoStart .~ False)) $
    flip R.runReaderT ibs $ 
      unIB ib

sendIB :: ServiceIn -> IB Bool
sendIB msg = ask >>= \(Service req _) -> (liftIO . atomically . send req) msg

recvIB :: IB (Maybe ServiceOut)
recvIB = do
  (Service _ resp) <- ask
  msg <- (liftIO . atomically . recv) resp 
  case msg of 
    Just (IBResponse (NextValidId oid)) -> do
      ibsNextOrderId .= Just oid 
      return msg
    _ -> return msg
      
connectIB :: IB Bool
connectIB = sendIB (IBServiceRequest ServiceStart) >> loop
  where
  loop = do
    msg <- recvIB 
    --liftIO $ print msg
    case msg of
      Just (IBResponse Connection{..}) -> do
        ibsConnectionStatus .= ServiceConnected
        loop
      Just (IBResponse (ManagedAccounts acs)) -> do
        ibsAccounts .= acs 
        loop
      Just (IBServiceStatus sts) -> do
        ibsServiceStatus .= sts
        if (sts == ServiceActive) 
          then return True 
          else loop
      _ -> loop

testIB :: IO ()
testIB = runIB def $ do
  connectIB
  forever $ recvIB >>= liftIO . print
