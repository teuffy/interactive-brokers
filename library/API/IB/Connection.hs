{-# LANGUAGE Arrows,DeriveDataTypeable,ImpredicativeTypes,OverloadedStrings,RankNTypes,RecordWildCards,ScopedTypeVariables,TemplateHaskell,TypeFamilies #-}

module API.IB.Connection (

    ServiceStatus(..)
  , ServiceCommand(..)
  , ServiceIn(..)
  , ServiceOut(..)
  , IBConfiguration(..)
  , ibService
  , withIB
  , cfgDebug

  ) where

import           Control.Applicative
import           Control.Arrow                    (arr, (>>>),(|||))
import           Control.Concurrent.Async         (withAsync)
import           Control.Lens                     hiding (view)
import           Control.Monad                    (forever,void,when)
import qualified Control.Monad.Trans.State.Strict as S
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8            as BC
import           Data.Default
import           Data.Map                         (Map)
import qualified Data.Map                         as Map (elems, empty,
                                                          fromList, keys, lookup)
import           Data.Monoid                      hiding (Last)
import           Data.Time.Zones
import           Data.Typeable
import           MVC
import           MVC.Event
import           MVC.Service
import           Pipes.Core

import           MVC.Socket                       hiding (Connection(..),ConnectionCommand(..),EventIn(..),EventOut(..),ServiceIn(..),ServiceOut(..),untilDone,_ServiceOut,_SocketOut)
import qualified MVC.Socket                       as K (ConnectionCommand(..),ServiceIn(..),ServiceOut(..))
import           Pipes.Edge

import           API.IB.Constant
import           API.IB.Data
import           API.IB.Request
import           API.IB.Util

-----------------------------------------------------------------------------

data ServiceIn = 
    IBServiceRequest ServiceCommand
  | IBRequest IBRequest
    deriving (Show,Typeable)

instance Event ServiceIn

data EventIn
  = ServiceIn ServiceIn
  | SocketIn K.ServiceOut
    deriving Show

data ServiceOut = 
    IBServiceStatus ServiceStatus
  | IBResponse IBResponse 
  | Log ByteString
    deriving (Show,Typeable)

instance Event ServiceOut

data EventOut =
    ServiceOut ServiceOut
  | SocketOut K.ServiceIn
  | Done
  deriving Show

makePrisms ''EventIn
makePrisms ''EventOut

type IBService = Service ServiceIn ServiceOut

-----------------------------------------------------------------------------

data IBConfiguration = IBConfiguration
  { _cfgAutoStart :: Bool --TODO
  , _cfgDebug :: Bool --TODO
  , _cfgClientId :: Int
  , _cfgConnRetryDelaySecs :: Int
  , _cfgSocketParams :: SocketParams
  , _cfgTimeZoneMap :: Map String String
  } 

instance Default IBConfiguration where 
  def = IBConfiguration 
    { _cfgAutoStart = True
    , _cfgDebug = False
    , _cfgClientId = 0
    , _cfgConnRetryDelaySecs = 10
    , _cfgSocketParams = SocketParams "127.0.0.1" "7496" False
    , _cfgTimeZoneMap = ibTimeZoneMap
    } 

data IBState = IBState
  { _ibsConfig      :: IBConfiguration
  , _ibsServiceStatus :: ServiceStatus
  , _ibsConnectionStatus :: ConnectionStatus
  , _ibsAccounts    :: [String]
  , _ibsNextOrderId :: Maybe Int
  , _ibsTimeZones :: Map String TZ
  } 

instance Default IBState where
  def = IBState def ServicePending ServiceDisconnected [] Nothing Map.empty

makeLenses ''IBConfiguration
makeLenses ''IBState

-----------------------------------------------------------------------------

eventHandler :: (Monad m) => Edge (S.StateT IBState m) () EventIn EventOut
eventHandler = proc e -> case e of
  ServiceIn (IBServiceRequest x)                     -> serviceRequestHandler -< x
  ServiceIn (IBRequest x)                            -> ibRequestHandler -< x 
  SocketIn (K.ServiceStatus x)                       -> socketStatusHandler -< x
  SocketIn (K.ConnectionStatus ServiceConnecting)    -> ignoreHandler -< ()
  SocketIn (K.ConnectionStatus ServiceConnected)     -> connectedHandler -< ()
  SocketIn (K.ConnectionStatus ServiceDisconnecting) -> ignoreHandler -< ()
  SocketIn (K.ConnectionStatus ServiceDisconnected)  -> disconnectedHandler -< ()
  SocketIn (K.Stream x)                              -> streamInHandler -< x
  SocketIn (K.Log x)                                 -> logHandler -< x

serviceRequestHandler :: (Monad m) => Edge (S.StateT IBState m) () ServiceCommand EventOut
serviceRequestHandler = proc e -> case e of  
  ServiceStart -> startupHandler -< ()
  ServicePause -> logHandler -< "ServicePause command not yet implemented"
  ServiceResume -> logHandler -< "ServiceResume command not yet implemented"
  ServiceStop -> shutdownHandler -< ()
  ServiceReportStatus -> serviceStatusRequestHandler -< e

startupHandler :: (Monad m) => Edge (S.StateT IBState m) () () EventOut
startupHandler = Edge $ push ~> \_ -> do
  updateServiceStatus ServiceActivating
  ibsConnectionStatus .= ServiceConnecting
  yield $ SocketOut $ K.ServiceCommand ServiceStart

shutdownHandler :: (Monad m) => Edge (S.StateT IBState m) () () EventOut
shutdownHandler = Edge $ push ~> \_ -> do
  s <- use ibsServiceStatus
  when (s /= ServiceTerminated) $ do
    ibsServiceStatus .= ServiceTerminating
    c <- use ibsConnectionStatus 
    if c `elem` [ServiceConnecting,ServiceConnected] then do
      ibsConnectionStatus .= ServiceDisconnecting
      yield $ SocketOut $ K.ServiceCommand ServiceStop
    else
      yield Done 

serviceStatusRequestHandler :: (Monad m) => Edge (S.StateT IBState m) () ServiceCommand EventOut
serviceStatusRequestHandler = Edge $ push ~> \_ -> do
  s <- use ibsServiceStatus
  yield $ ServiceOut $ IBServiceStatus s

ibRequestHandler :: (Monad m) => Edge (S.StateT IBState m) () IBRequest EventOut
ibRequestHandler = Edge $ push ~> \e -> do
  s <- use ibsServiceStatus
  if s == ServiceActive then
    case createMsg e of
      Nothing -> yield $ ServiceOut $ Log "Cannot create request message"
      Just m -> yield $ SocketOut $ K.Send m
  else
    yield $ ServiceOut $ Log "IB request submitted while service inactive"

socketStatusHandler :: (Monad m) => Edge (S.StateT IBState m) () ServiceStatus EventOut
socketStatusHandler = proc e -> case e of
  ServicePending -> logHandler -< "Service pending handler not yet implemented"
  ServiceActivating -> logHandler -< "Service activating handler not yet implemented"
  ServiceActive -> logHandler -< "Service active handler not yet implemented"
  ServiceTerminating -> logHandler -< "Service terminating handler not yet implemented"
  ServiceTerminated -> logHandler -< "Service terminated handler not yet implemented"

streamInHandler :: (Monad m) => Edge (S.StateT IBState m) () ByteString EventOut
streamInHandler = debugHandler >>> (arr id ||| (parseHandler >>> streamHandler))
  where
  debugHandler = Edge $ \e -> do 
    dbg <- use (ibsConfig . cfgDebug)
    push e >-> forever (do
      e' <- await
      when dbg $ yield $ Left $ ServiceOut $ Log e'
      yield $ Right e')
  parseHandler = Edge $ \e -> push e >-> parseP parseIBResponses (mapM_ yield)

streamHandler :: (Monad m) => Edge (S.StateT IBState m) () (Either String IBResponse) EventOut
streamHandler = Edge $ push ~> \e -> 
  case e of
    Left b -> 
      yield $ ServiceOut $ Log $ BC.append "Cannot parse ib response: " $ BC.pack b
    Right r -> do
      r' <- case r of
        Connection{..} -> do
          ibsConnectionStatus .= ServiceConnected
          cid <- use (ibsConfig . cfgClientId)
          yield $ SocketOut $ K.Send $ createClientIdMsg cid
          tzs <- use ibsTimeZones
          return $ r & connServerTimeZone .~ Map.lookup _connServerTimeZoneDesc tzs
        ManagedAccounts acs -> do
          ibsAccounts .= acs 
          return r
        NextValidId oid -> do
          ibsNextOrderId .= Just oid 
          s <- use ibsServiceStatus
          when (s == ServiceActivating) $ updateServiceStatus ServiceActive
          return r
        _ -> return r
      yield $ ServiceOut $ IBResponse r'

logHandler :: (Monad m) => Edge (S.StateT IBState m) () ByteString EventOut
logHandler = arr (ServiceOut . Log)

connectedHandler :: (Monad m) => Edge (S.StateT IBState m) () () EventOut
connectedHandler = arr (const $ SocketOut $ K.Send $ createClientVersionMsg clientVersion)

disconnectedHandler :: (Monad m) => Edge (S.StateT IBState m) () () EventOut
disconnectedHandler = Edge $ push ~> \_ -> do
  s <- use ibsServiceStatus
  if s `elem` [ServiceActivating, ServiceActive] then do
    d <- use (ibsConfig . cfgConnRetryDelaySecs)
    updateServiceStatus ServiceActivating     
    ibsConnectionStatus .= ServiceConnecting
    yield $ SocketOut $ K.ConnectionCommand $ K.Connect d
  else if s == ServiceTerminating then do
    ibsConnectionStatus .= ServiceDisconnected
    yield Done
  else
    yield $ ServiceOut $ Log "Unexpected"

--finishedHandler :: (Monad m) => Edge (S.StateT IBState m) () () EventOut
--finishedHandler = Edge $ push ~> \_ -> do
--  s <- use ibsServiceStatus
--  if s == ServiceTerminating then do
--    updateServiceStatus ServiceTerminated
--    yield Done
--  else
--    yield $ ServiceOut $ Log "Unexpected"

ignoreHandler :: (Monad m) => Edge (S.StateT IBState m) () () EventOut
ignoreHandler = Edge $ push ~> \_ -> return ()

updateServiceStatus :: Monad m => ServiceStatus -> Pipe a EventOut (S.StateT IBState m) ()
updateServiceStatus s = do
  ibsServiceStatus .= s 
  yield $ ServiceOut $ IBServiceStatus s 

-----------------------------------------------------------------------------

untilDone :: Model IBState EventOut EventOut
untilDone = asPipe go
  where
  go = do
    e <- await
    yield e
    case e of
      Done -> return ()
      _    -> go

-----------------------------------------------------------------------------

ibService :: IBConfiguration -> Managed IBService
ibService conf@IBConfiguration{..} = do
  
  (vSocket,cSocket) <- toManagedMVC $ toManagedService $ socketService _cfgSocketParams

  managed $ \k -> do

    (vServiceIn , cServiceIn , sServiceIn)  <- spawn' Unbounded
    (vServiceOut, cServiceOut, sServiceOut) <- spawn' Unbounded
  
    let

      sealAll :: IO ()
      sealAll = atomically $ sServiceIn >> sServiceOut

      view :: View EventOut
      view = mconcat
        [ handles _ServiceOut (asSink $ void . atomically . send vServiceOut)
        , handles _SocketOut vSocket
        ]

      controller :: Controller EventIn
      controller = mconcat
        [ ServiceIn <$> asInput cServiceIn
        , SocketIn  <$> cSocket
        ]

      external :: Managed (View EventOut, Controller EventIn)
      external = return (view,controller)

      model :: Model IBState EventIn EventOut
      model = 
        asPipe (yield (ServiceIn (IBServiceRequest ServiceStart)) >> cat)
        >>> asPipe (runEdge eventHandler)
        >>> untilDone

      io = do
        tzs <- loadTimeZones $ Map.elems _cfgTimeZoneMap
        let tzs' = Map.fromList $ zip (Map.keys _cfgTimeZoneMap) tzs
        let initialState = def & (ibsConfig .~ conf) & (ibsTimeZones .~ tzs')
        runMVC initialState model external

    withAsync io $ \_ -> k (Service vServiceIn cServiceOut) <* sealAll

-----------------------------------------------------------------------------

type IBServiceInOut = Either ServiceIn ServiceOut 

withIB :: IBConfiguration -> (IBService -> IO x) -> IO x
withIB conf k = do

  (vServiceIn , cServiceIn , sServiceIn)  <- spawn' Unbounded
  (vServiceOut, cServiceOut, sServiceOut) <- spawn' Unbounded

  let

    sealAll :: IO ()
    sealAll = atomically $ sServiceIn >> sServiceOut

    external :: Managed (View IBServiceInOut, Controller IBServiceInOut)
    external = do
      (ibV,ibC) <- toManagedMVC $ toManagedService $ ibService conf
      let 
        view = mconcat 
          [ handles _Left ibV
          , handles _Right (asSink $ void . atomically . send vServiceOut)
          ]
        controller = mconcat 
          [ Left <$> asInput cServiceIn
          , Right <$> ibC
          ]
      return (view,controller)

    io = runMVC () (asPipe cat) external

  withAsync io $ \_ -> k (Service vServiceIn cServiceOut) <* sealAll






