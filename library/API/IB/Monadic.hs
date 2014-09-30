{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module API.IB.Monadic 

  ( IB
  , runIB
  , status
  , accounts
  , connection
  , nextOrderId
  , nextRequestId
  , send
  , recv
  , connect
  , disconnect
  , stop
  , requestMarketData
  , placeOrder
  , testIB
  ) where

import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader             (asks,MonadReader (..))
import           Control.Monad.State              (MonadState (..))
import           Control.Monad.Trans.Reader       (ReaderT)
import qualified Control.Monad.Trans.Reader       as R
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as S
import           Data.Default
import           MVC                              hiding (loop,recv,send)
import qualified MVC                              as M (recv,send)
import           MVC.Service
import           System.Console.Haskeline

import           API.IB.Connection
import           API.IB.Data
import           API.IB.Enum

-----------------------------------------------------------------------------

data IBState = IBState
  { _ibsServiceStatus :: ServiceStatus
  , _ibsAccounts :: [String]
  , _ibsConnection :: Maybe IBConnection 
  , _ibsNextOrderId :: Maybe Int
  , _ibsNextRequestId :: Int
  }

makeLenses ''IBState

instance Default IBState where 
  def = IBState
    { _ibsServiceStatus = ServicePending
    , _ibsAccounts = []
    , _ibsConnection = Nothing
    , _ibsNextOrderId = Nothing
    , _ibsNextRequestId = 1
    }  

-----------------------------------------------------------------------------

newtype IB r = 
  IB { unIB :: ReaderT IBService (StateT IBState IO) r}
  deriving (Functor,Applicative,Monad,MonadException,MonadIO,MonadReader IBService,MonadState IBState)

runIB :: IBConfiguration -> IB a -> IO a
runIB cfg ib = withIB (cfg & cfgAutoStart .~ False & cfgDebug .~ True) $ \ibs -> 
  flip S.evalStateT def $ flip R.runReaderT ibs $ unIB ib

-----------------------------------------------------------------------------

status :: IB ServiceStatus
status = use ibsServiceStatus

accounts :: IB [String]
accounts = use ibsAccounts

connection :: IB (Maybe IBConnection)
connection = use ibsConnection 

serverVersion :: IB (Maybe Int)
serverVersion = fmap (view connServerVersion) <$> connection

nextOrderId :: IB (Maybe Int)
nextOrderId = use ibsNextOrderId

nextRequestId :: IB Int
nextRequestId = use ibsNextRequestId

-----------------------------------------------------------------------------

send :: ServiceIn -> IB Bool
send msg = do
  req <- asks request -- use lens when Service updated
  sent <- liftIO $ atomically $ M.send req msg
  case msg of 
    IBRequest msg' -> do
      when (sent && updateRequestId msg') $ ibsNextRequestId %= (+1)
      return True
    _ -> 
      return sent

recv :: IB (Maybe ServiceOut)
recv = do
  resp <- asks response -- use lens when Service updated
  msg <- (liftIO . atomically . M.recv) resp 
  case msg of 
    Just (IBResponse (Connection c@IBConnection{})) -> 
      ibsConnection .= Just c
    Just (IBResponse (ManagedAccounts acs)) ->
        ibsAccounts .= acs 
    Just (IBResponse (NextValidId oid)) ->
      ibsNextOrderId .= Just oid 
    Just (IBServiceStatus sts) -> do
      unless (sts == ServiceActive) $ put def 
      ibsServiceStatus .= sts
    _ -> return ()
  return msg  

-----------------------------------------------------------------------------

connect :: IB (Maybe IBConnection)
connect = send (IBServiceRequest ServiceStart) >> loop
  where
  loop = do
    msg <- recv 
    liftIO $ print msg
    case msg of
      Nothing -> return Nothing
      Just (IBServiceStatus sts) ->
        if sts == ServiceActive
          then connection
          else loop
      _ -> loop

disconnect :: IB ()
disconnect = void $ send (IBServiceRequest ServicePause)

stop :: IB ()
stop = send (IBServiceRequest ServiceStop) >> loop
  where
  loop = recv >>= \case
    Just (IBServiceStatus ServiceTerminated) -> return ()
    _ -> loop

-----------------------------------------------------------------------------

requestMarketData :: IBContract -> [IBGenericTickType] -> Bool -> IB Bool
requestMarketData contract genticktypes snapshot = do
  sent <- runMaybeT $ do
    sv <- MaybeT serverVersion
    rid <- lift nextRequestId
    lift $ send $ IBRequest $ RequestMarketData sv rid contract genticktypes snapshot
  maybe (return False) return sent

placeOrder :: IBContract -> IBOrder -> IB Bool
placeOrder contract order = do
  sent <- runMaybeT $ do 
    sv <- MaybeT serverVersion
    oid <- MaybeT nextOrderId
    lift $ send $ IBRequest $ PlaceOrder sv oid contract order
  maybe (return False) return sent
  
-----------------------------------------------------------------------------

testIB :: IO ()
testIB = runIB def $ connect >> forever (recv >>= liftIO . print)
