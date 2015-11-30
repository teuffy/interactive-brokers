{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module API.IB.Connection 

  ( ServiceStatus(..)
  , ServiceCommand(..)
  , ServiceIn(..)
  , ServiceOut(..)
  , IBConfiguration(..)
  , ibService
  , IBService
  , withIB
  , cfgAutoStart
  ) where

import           Control.Applicative
import           Control.Arrow                    (arr, (>>>),(|||))
import           Control.Concurrent.Async         (Async,cancel,wait,withAsync)
import           Control.Exception
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
import qualified System.Log.Logger                as L
import           System.Log.Logger.TH             (deriveLoggers)

import qualified MVC.Socket                       as K
import           Pipes.Edge

import           API.IB.Constant
import           API.IB.Data
import           API.IB.Request
import           API.IB.Util

-----------------------------------------------------------------------------

$(deriveLoggers "L" [L.DEBUG])

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
  { _cfgAutoStart :: Bool
  , _cfgClientId :: Int
  , _cfgConnRetryDelaySecs :: Int
  , _cfgSocketParams :: K.SocketParams
  , _cfgTimeZones :: Map String String
  } 

instance Default IBConfiguration where 
  def = IBConfiguration 
    { _cfgAutoStart = True
    , _cfgClientId = 23
    , _cfgConnRetryDelaySecs = 10
    , _cfgSocketParams = K.SocketParams "192.168.1.106" "7496" -- "7496"
    , _cfgTimeZones = ibTimeZones
    } 

data IBState = IBState
  { _ibsConfig :: IBConfiguration
  , _ibsConnectionStatus :: ConnectionStatus
  , _ibsDebug :: Bool
  , _ibsServiceStatus :: ServiceStatus
  , _ibsTimeZones :: Map String TZ
  } 

instance Default IBState where 
  def = IBState 
    { _ibsConfig = def
    , _ibsServiceStatus = ServicePending
    , _ibsDebug = False
    , _ibsConnectionStatus = ServiceDisconnected
    , _ibsTimeZones = Map.empty
    }

makeLenses ''IBConfiguration
makeLenses ''IBState

-----------------------------------------------------------------------------

eventHandler :: (Monad m) => Edge (S.StateT IBState m) () EventIn EventOut
eventHandler = proc e -> case e of
  ServiceIn (IBServiceRequest x) -> serviceRequestHandler -< x
  ServiceIn (IBRequest x)        -> ibRequestHandler -< x 
  SocketIn (K.ServiceStatus x)   -> socketStatusHandler -< x
  SocketIn (K.Stream x)          -> streamInHandler -< x
  SocketIn (K.Log x)             -> logHandler -< x

socketStatusHandler :: (Monad m) => Edge (S.StateT IBState m) () ServiceStatus EventOut
socketStatusHandler = proc e -> case e of 
  ServiceActive -> connectedHandler -< ()
  ServicePending -> disconnectedHandler -< ()
  ServiceTerminating -> ignoreHandler -< ()
  ServiceTerminated -> disconnectedHandler -< ()
  _ -> ignoreHandler -< ()

serviceRequestHandler :: (Monad m) => Edge (S.StateT IBState m) () ServiceCommand EventOut
serviceRequestHandler = proc e -> case e of  
  ServiceStart -> startupHandler -< ()
  ServicePause -> pauseHandler -< ()
  ServiceResume -> startupHandler -< ()
  ServiceStop -> shutdownHandler -< ()
  ServiceReportStatus -> serviceStatusRequestHandler -< e

startupHandler :: (Monad m) => Edge (S.StateT IBState m) () () EventOut
startupHandler = Edge $ push ~> \_ -> do
  s <- use ibsServiceStatus
  if s == ServicePending then do
    c <- use ibsConnectionStatus 
    if c `elem` [ServiceDisconnecting,ServiceDisconnected] then do
      updateServiceStatus ServiceActivating
      ibsConnectionStatus .= ServiceConnecting
      yield $ SocketOut $ K.ServiceCommand ServiceStart
    else 
      updateServiceStatus $ if c == ServiceConnected then ServiceActive else ServiceActivating
  else
    yield $ ServiceOut $ Log "IB service start command submitted while service not pending"

pauseHandler :: (Monad m) => Edge (S.StateT IBState m) () () EventOut
pauseHandler = Edge $ push ~> \_ -> do
  s <- use ibsServiceStatus
  if s == ServiceActive then do
    c <- use ibsConnectionStatus 
    if c `elem` [ServiceConnecting,ServiceConnected] then do
      updateServiceStatus ServicePausing
      ibsConnectionStatus .= ServiceDisconnecting
      yield $ SocketOut $ K.ServiceCommand ServicePause    
    else 
      updateServiceStatus ServicePending
  else
      yield $ ServiceOut $ Log "IB service pause command submitted while service not active"

shutdownHandler :: (Monad m) => Edge (S.StateT IBState m) () () EventOut
shutdownHandler = Edge $ push ~> \_ -> do
  s <- use ibsServiceStatus
  if s `elem` [ServiceTerminating,ServiceTerminated] then
    yield (ServiceOut $ Log "IB service stop command submitted while service terminated")
  else do
    c <- use ibsConnectionStatus 
    if c `elem` [ServiceConnecting,ServiceConnected] then do
      updateServiceStatus ServiceTerminating
      ibsConnectionStatus .= ServiceDisconnecting
      yield $ SocketOut $ K.ServiceCommand ServiceStop
    else do
      updateServiceStatus ServiceTerminated
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
      Left err -> yield $ ServiceOut $ Log $ "Cannot create request message: " <> BC.pack (show err)
      Right m -> yield $ SocketOut $ K.Send m
  else
    yield $ ServiceOut $ Log "IB request submitted while service inactive"

streamInHandler :: (Monad m) => Edge (S.StateT IBState m) () ByteString EventOut
streamInHandler = debugHandler >>> (arr id ||| (parseHandler >>> streamHandler))
  where
  debugHandler = Edge $ \e -> do 
    dbg <- use ibsDebug
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
        Connection IBConnection{..} -> do
          ibsConnectionStatus .= ServiceConnected
          cid <- use (ibsConfig . cfgClientId)
          yield $ SocketOut $ K.Send $ createClientIdMsg cid
          tzs <- use ibsTimeZones
          return $ r & (connConnection . connServerTimeZone) .~ Map.lookup _connServerTimeZoneDesc tzs
        NextValidId{} -> do
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
  else if s == ServicePausing then do
    updateServiceStatus ServicePending
    ibsConnectionStatus .= ServiceDisconnected
  else if s == ServiceTerminating then do
    updateServiceStatus ServiceTerminated
    ibsConnectionStatus .= ServiceDisconnected
    yield Done
  else
    yield $ ServiceOut $ Log "Unexpected"

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
      Done -> do
        yield $ ServiceOut $ Log "Connection Done"
        return ()
      _    -> go

-----------------------------------------------------------------------------

ibService :: IBConfiguration -> Managed IBService
ibService conf@IBConfiguration{..} = do
  
  (vSocket,cSocket) <- toManagedMVC $ toManagedService $
    K.socketService $ def & K.scSocketParams .~ _cfgSocketParams

  managed $ \k -> do

    (vServiceIn , cServiceIn , sServiceIn)  <- spawn' Unbounded
    (vServiceOut, cServiceOut, sServiceOut) <- spawn' Unbounded
  
    let

      sealAll :: IO ()
      sealAll = atomically $ sServiceIn >> sServiceOut

      view :: View EventOut
      view = mconcat
        --[ asSink (putStrLn . ("ibService view: " ++) . show)
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
        start
        >>> asPipe (runEdge eventHandler)
        >>> untilDone

      start :: Model IBState EventIn EventIn
      start = asPipe $ do
        autoStart <- lift $ use (ibsConfig . cfgAutoStart)
        when autoStart $ yield $ ServiceIn $ IBServiceRequest ServiceStart
        cat

      stop :: Async () -> IO ()
      stop a = do
        debugM "ibService stop"
        --void $ atomically $ send vServiceIn $ IBServiceRequest ServiceStop
        cancel a
        sealAll

      errorHandler :: SomeException -> IO ()
      errorHandler e = do
        debugM $ "ibService error: " ++ show e
        --TODO: take action depending on whehther async (ThreadKilled)
        --or sync exception
        throwIO e   

      io :: IO ()
      io = handle errorHandler $ do
        debug <- ((== Just L.DEBUG) . L.getLevel) <$> L.getRootLogger
        tzs <- loadTimeZones $ Map.elems _cfgTimeZones
        let tzs' = Map.fromList $ zip (Map.keys _cfgTimeZones) tzs
        let initialState = def & (ibsConfig .~ conf) & (ibsDebug .~ debug) & (ibsTimeZones .~ tzs')
        void $ runMVC initialState model external
        debugM "ibService end"

    withAsync io $ \a -> k (Service vServiceIn cServiceOut) <* stop a

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
      --tickC <- tick 1
      let 
        view = mconcat 
          --[ asSink (putStrLn . ("withIB view: " ++) . show)
          [ handles _Left ibV
          , handles _Right (asSink $ void . atomically . send vServiceOut)
          ]
        controller = mconcat 
          [ Left <$> asInput cServiceIn
          , Right <$> ibC
          --, const (Right $ Log "tick" ) <$> tickC
          ]
      return (view,controller)
      
    model :: Model () IBServiceInOut IBServiceInOut
    model = asPipe cat

    stop :: Async () -> IO ()
    stop a = do
      debugM "withIB stop"
      --runMVC () (asPipe $ yield $ Left $ IBServiceRequest ServiceStop) external
      wait a
      sealAll
    
    errorHandler :: SomeException -> IO ()
    errorHandler e = do
      debugM $ "withIB error: " ++ show e
      --TODO: take action depending on whehther async (ThreadKilled)
      --or sync exception
      throwIO e   

    io :: IO ()
    io = handle errorHandler $ do
      runMVC () model external
      debugM "withIB end"

  withAsync io $ \a -> k (Service vServiceIn cServiceOut) <* stop a





