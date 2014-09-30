{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module MVC.Socket where

import           Control.Applicative
import           Control.Category          ((>>>))
import           Control.Concurrent        (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens              hiding (view)
import           Control.Monad             hiding (forM_)
import           Data.ByteString           (ByteString)
import           Data.Default
import           Data.Foldable             (forM_)
import           MVC
import           MVC.Service
import qualified Network                   as N
import qualified Network.Simple.TCP        as S
import           Pipes.Network.TCP         (fromSocket,Socket)

-----------------------------------------------------------------------------

data ConnectionCommand = 
    Connect Int
  | Disconnect
    deriving Show

data ServiceIn = 
    ServiceCommand ServiceCommand
  | ConnectionCommand ConnectionCommand  
  | Send ByteString
    deriving Show

data EventIn = 
    ServiceIn ServiceIn
  | SocketIn ByteString
  | ConnectionIn Bool
    deriving Show

data ServiceOut = 
    ServiceStatus ServiceStatus
  | Stream ByteString
  | Log ByteString
    deriving Show  

data EventOut =
    ServiceOut ServiceOut
  | SocketOut ByteString
  | ConnectionOut ConnectionCommand
  | Done
    deriving Show

makePrisms ''EventIn
makePrisms ''EventOut

type SocketService = Service ServiceIn ServiceOut

-----------------------------------------------------------------------------

data SocketServiceState = SocketServiceState
  { _ssServiceStatus :: ServiceStatus
  , _ssConnectionStatus :: ConnectionStatus
  } 

instance Default SocketServiceState where
  def = SocketServiceState ServicePending ServiceDisconnected

makeLenses ''SocketServiceState

-----------------------------------------------------------------------------

data SocketParams = SocketParams
    { _host      :: N.HostName
    , _port      :: String
    , _useSecure :: Bool
    } deriving (Show, Eq)

newtype Connection = Connection { _unConnection :: TVar (Maybe Socket) }

newEmptyConnection :: IO Connection
newEmptyConnection = Connection <$> newTVarIO Nothing

newConnection :: Socket -> IO Connection
newConnection sock = Connection <$> newTVarIO (Just sock)

getSocket :: Connection -> STM (Maybe Socket)
getSocket conn = readTVar $ _unConnection conn

putSocket :: Connection -> Socket -> STM ()
putSocket conn sock = writeTVar (_unConnection conn) (Just sock) 

openConnection :: Connection -> SocketParams -> IO Bool
openConnection conn p = do 
  closeConnection conn
  r <- connectSocket p
  either (const $ return False) connection' r
  where
  connection' (sock,_) = atomically (putSocket conn sock) >> return True

closeConnection :: Connection -> IO ()
closeConnection conn = do
  sock <- atomically (getSocket conn)
  forM_ sock S.closeSock
  atomically $ clearSocket conn 

clearSocket :: Connection -> STM ()
clearSocket conn = writeTVar (_unConnection conn) Nothing

connection :: SocketParams -> IO (Maybe Connection)
connection p = connectSocket p >>= either (const $ return Nothing) connection'
  where
  connection' (sock,_) = Just <$> newConnection sock

connectSocket :: SocketParams -> IO (Either SomeException (S.Socket,S.SockAddr))
connectSocket p = try $ S.connectSock (_host p) (_port p)

-----------------------------------------------------------------------------

connectionReader :: Buffer ByteString -> Connection -> Int -> Output Bool -> Managed (Controller ByteString)
connectionReader buffer conn nbytes status = managed $ \k -> do
  
  (vOut, cOut, sOut) <- spawn' buffer
  
  let 

    sealAll :: IO ()
    sealAll = atomically sOut

    stop :: Async () -> IO ()
    stop a = do
      wait a
      sealAll

    streamErrorHandler :: SomeException -> IO ()
    streamErrorHandler _ = do
      putStrLn "connectionReader stream error"
      return ()

    stream :: Socket -> IO ()
    stream sock = handle streamErrorHandler $ do
      runEffect $ fromSocket sock nbytes >-> toOutput vOut
      void $ atomically $ clearSocket conn >> send status False

    ioErrorHandler :: SomeException -> IO ()
    ioErrorHandler e = do
      putStrLn "connectionReader error"
      sealAll
      throwIO e

    io :: IO ()
    io = handle ioErrorHandler $ forever $ do
      sock <- atomically $ getSocket conn >>= maybe retry return
      void $ async $ stream sock
      void $ atomically $ getSocket conn >>= maybe (return ()) (const retry)

  withAsync io $ \a -> k (asInput cOut) <* stop a

-----------------------------------------------------------------------------

connectionWriter :: Connection -> Output Bool -> ByteString -> IO ()
connectionWriter conn status bs =
  atomically (getSocket conn) >>= maybe (return ()) (send' bs)
  where
  send' :: ByteString -> Socket -> IO ()  
  send' bs' sock = send'' sock bs' >>= either (const disconnected) return  
  send'' :: Socket -> ByteString -> IO (Either SomeException ())
  send'' sock bs' = try $ S.send sock bs'
  disconnected :: IO ()
  disconnected = void $ atomically $ clearSocket conn >> send status False

-----------------------------------------------------------------------------

connectionHandler :: SocketParams -> Connection -> Output Bool -> ConnectionCommand -> IO ()
connectionHandler p conn status cmd = case cmd of
  Connect d -> threadDelay (d * 1000000) >> openConnection conn p >>= void . atomically . send status
  Disconnect -> closeConnection conn >> void (atomically $ send status False)

socketServiceModel :: Model SocketServiceState EventIn EventOut
socketServiceModel = asPipe (loop model) >>> untilDone
  where
  model event = Select $
    case event of
      (ServiceIn (ServiceCommand ServiceStart))        -> yield (ConnectionOut (Connect 0))
      (ServiceIn (ServiceCommand ServicePause))        -> yield (ConnectionOut Disconnect) 
      (ServiceIn (ServiceCommand ServiceResume))       -> yield (ConnectionOut (Connect 0))
      (ServiceIn (ServiceCommand ServiceStop))         -> yield Done
      (ServiceIn (ServiceCommand ServiceReportStatus)) -> lift (use ssServiceStatus) >>= yield . ServiceOut . ServiceStatus
      (ServiceIn (ConnectionCommand c))                -> yield (ConnectionOut c)
      (ServiceIn (Send bs))                            -> yield (SocketOut bs)
      (SocketIn bs)                                    -> yield (ServiceOut (Stream bs))
      (ConnectionIn connected)                         -> do
        let ss = connected' connected
        lift $ ssServiceStatus .= ss
        yield (ServiceOut (ServiceStatus ss))
  connected' True = ServiceActive
  connected' False = ServicePending

untilDone :: Model SocketServiceState EventOut EventOut
untilDone = asPipe go
  where
  go = do
    e <- await
    case e of
      Done -> void $ do
        yield (ServiceOut $ Log "Socket Done")
        yield (ServiceOut $ ServiceStatus ServiceTerminating)
        yield (ConnectionOut Disconnect)
        yield (ServiceOut $ ServiceStatus ServiceTerminated)
      _ -> yield e >> go

socketService :: SocketParams -> Managed SocketService
socketService p = do

  conn <- managed $ \k -> newEmptyConnection >>= k

  (vConnection, cConnection, sConnection) <- managed $ \k -> spawn' Unbounded >>= k

  cSocketIn <- connectionReader Single conn 4096 vConnection

  managed $ \k -> do

    (vServiceIn , cServiceIn , sServiceIn)  <- spawn' Unbounded
    (vServiceOut, cServiceOut, sServiceOut) <- spawn' Unbounded
  
    let

      sealAll :: IO ()
      sealAll = atomically $ sServiceIn >> sServiceOut >> sConnection

      view :: View EventOut
      view = mconcat
        [ asSink (putStrLn . ("socketService view: " ++) . show)
        , handles _ServiceOut (asSink $ void . atomically . send vServiceOut)
        , handles _SocketOut (asSink $ connectionWriter conn vConnection)
        , handles _ConnectionOut (asSink $ connectionHandler p conn vConnection)
        ]

      controller :: Controller EventIn
      controller = mconcat
        [ ServiceIn <$> asInput cServiceIn
        , SocketIn <$> cSocketIn
        , ConnectionIn <$> asInput cConnection
        ]

      external :: Managed (View EventOut, Controller EventIn)
      external = return (view,controller)

      errorHandler :: SomeException -> IO ()
      errorHandler e = do
        putStrLn $ "Debug: socketService error: " ++ show e
        --TODO: take action depending on whehther async (ThreadKilled)
        --or sync exception
        closeConnection conn
        sealAll
        throwIO e  

      stop :: Async () -> IO ()
      stop a = do
        putStrLn "socketService stop"
        closeConnection conn
        wait a
        sealAll
      
      io :: IO ()
      io = do
        handle errorHandler $ void $ runMVC def socketServiceModel external
        putStrLn "Debug: socketService finished"

    withAsync io $ \a -> k (Service vServiceIn cServiceOut) <* stop a

