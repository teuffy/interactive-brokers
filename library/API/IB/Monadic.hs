{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , connect
  , disconnect
  , send
  , recv
  , testIB
  ) where

import           Control.Applicative
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
runIB cfg ib = withIB cfg $ \ibs -> 
  flip S.evalStateT def $ flip R.runReaderT ibs $ unIB ib

-----------------------------------------------------------------------------

status :: IB ServiceStatus
status = use ibsServiceStatus

accounts :: IB [String]
accounts = use ibsAccounts

connection :: IB (Maybe IBConnection)
connection = use ibsConnection 

nextOrderId :: IB (Maybe Int)
nextOrderId = use ibsNextOrderId

nextRequestId :: IB Int
nextRequestId = use ibsNextRequestId

-----------------------------------------------------------------------------

connect :: IB Bool
connect = send (IBServiceRequest ServiceStart) >> loop
  where
  loop = do
    msg <- recv 
    --liftIO $ print msg
    case msg of
      Just (IBResponse (Connection c@IBConnection{})) -> do
        ibsConnection .= Just c
        loop
      Just (IBResponse (ManagedAccounts acs)) -> do
        ibsAccounts .= acs 
        loop
      Just (IBServiceStatus sts) -> do
        ibsServiceStatus .= sts
        if sts == ServiceActive
          then return True 
          else loop
      _ -> loop

disconnect :: IB Bool
disconnect = undefined

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
    Just (IBResponse (NextValidId oid)) -> do
      ibsNextOrderId .= Just oid 
      return msg
    _ -> return msg
      
-----------------------------------------------------------------------------

testIB :: IO ()
testIB = runIB def $ connect >> forever (recv >>= liftIO . print)
