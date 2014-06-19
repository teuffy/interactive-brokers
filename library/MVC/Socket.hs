{-# LANGUAGE DeriveDataTypeable,ScopedTypeVariables,TemplateHaskell #-}

module MVC.Socket where

import           Control.Applicative
import           Control.Category          ((>>>))
import           Control.Concurrent        (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens              (makeLenses,makePrisms,use)
import           Control.Monad             (forever,void)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B
import           Data.Default
import           Data.Foldable             (forM_)
import           MVC
import           MVC.Prelude
import           MVC.Service
import qualified Network                   as N
import qualified Network.Simple.TCP        as S
import qualified Network.Socket.ByteString as NSB
import           Pipes.Network.TCP         (Socket)

-- -----------------------------------------------------------------------------
-- 

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
  | ConnectionStatus ConnectionStatus
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

-- -----------------------------------------------------------------------------
-- 

data SocketServiceState = SocketServiceState
  { _ssServiceStatus :: ServiceStatus
  , _ssConnectionStatus :: ConnectionStatus
  } 

instance Default SocketServiceState where
  def = SocketServiceState ServicePending ServiceDisconnected

makeLenses ''SocketServiceState

-- -----------------------------------------------------------------------------
-- 

data SocketParams = SocketParams
    { _host      :: N.HostName
    , _port      :: String
    , _useSecure :: Bool
    } deriving (Show, Eq)

newtype Connection = Connection { _unConnection :: TVar (Maybe Socket) }

newEmptyConnection :: IO Connection
newEmptyConnection = newTVarIO Nothing >>= return . Connection

newConnection :: Socket -> IO Connection
newConnection sock = newTVarIO (Just sock) >>= return . Connection

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
  forM_ sock N.sClose
  atomically $ clearSocket conn 

clearSocket :: Connection -> STM ()
clearSocket conn = writeTVar (_unConnection conn) Nothing

connection :: SocketParams -> IO (Maybe Connection)
connection p = connectSocket p >>= either (const $ return Nothing) connection'
  where
  connection' (sock,_) = newConnection sock >>= return . Just 

connectSocket :: SocketParams -> IO (Either SomeException (S.Socket,S.SockAddr))
connectSocket p = try $ S.connectSock (_host p) (_port p)

-- -----------------------------------------------------------------------------
-- 

connectionReader :: Connection -> Int -> Output Bool -> Producer ByteString IO ()
connectionReader conn nbytes status = forever $ do
  sock <- liftIO $ atomically $ getSocket conn >>= maybe retry return 
  bs <- liftIO $ NSB.recv sock nbytes
  if B.null  bs
    then void $ liftIO $ atomically $ clearSocket conn >> send status False
    else yield bs

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

-- -----------------------------------------------------------------------------
-- 

connectionHandler :: SocketParams -> Connection -> Output Bool -> ConnectionCommand -> IO ()
connectionHandler p conn status cmd = case cmd of
  Connect d -> threadDelay (d * 1000000) >> openConnection conn p >>= void . atomically . send status
  Disconnect -> closeConnection conn >> void (atomically $ send status False)

socketServiceModel :: Model SocketServiceState EventIn EventOut
socketServiceModel = asPipe (loop model) >>> untilDone
  where
  model event = Select $ do
    case event of
      (ServiceIn (ServiceCommand ServiceStart))        -> yield (ConnectionOut (Connect 0))
      (ServiceIn (ServiceCommand ServicePause))        -> yield (ConnectionOut Disconnect) 
      (ServiceIn (ServiceCommand ServiceResume))       -> yield (ConnectionOut (Connect 0))
      (ServiceIn (ServiceCommand ServiceStop))         -> yield Done
      (ServiceIn (ServiceCommand ServiceReportStatus)) -> lift (use ssServiceStatus) >>= yield . ServiceOut . ServiceStatus
      (ServiceIn (ConnectionCommand c))                -> yield (ConnectionOut c)
      (ServiceIn (Send bs))                            -> yield (SocketOut bs)
      (SocketIn bs)                                    -> yield (ServiceOut (Stream bs))
      (ConnectionIn connected)                         -> yield (ServiceOut (ConnectionStatus (connected' connected)))
  connected' True = ServiceConnected
  connected' False = ServiceDisconnected

untilDone :: Model SocketServiceState EventOut EventOut
untilDone = asPipe go
  where
  go = do
    e <- await
    case e of
      Done -> yield (ConnectionOut Disconnect) >> return ()
      _    -> yield e >> go

socketService :: SocketParams -> Managed SocketService
socketService p = do

  conn <- managed $ \k -> newEmptyConnection >>= k

  (vConnection, cConnection, sConnection) <- managed $ \k -> spawn' Unbounded >>= k

  cSocketIn <- producer Single (connectionReader conn 4096 vConnection)

  managed $ \k -> do

    (vServiceIn , cServiceIn , sServiceIn)  <- spawn' Unbounded
    (vServiceOut, cServiceOut, sServiceOut) <- spawn' Unbounded
  
    let

      sealAll :: IO ()
      sealAll = atomically $ sServiceIn >> sServiceOut >> sConnection

      view :: View EventOut
      view = mconcat
        [ handles _ServiceOut (asSink $ void . atomically . send vServiceOut)
        , handles _SocketOut (asSink $ connectionWriter conn vConnection)
        , handles _ConnectionOut (asSink $ connectionHandler p conn vConnection)
        ]

      controller :: Controller EventIn
      controller = mconcat
        [ ServiceIn <$> (asInput cServiceIn)
        , SocketIn  <$> cSocketIn
        , ConnectionIn <$> (asInput cConnection)
        ]

      external :: Managed (View EventOut, Controller EventIn)
      external = return (view,controller)

      io = runMVC def socketServiceModel external

    withAsync io $ \_ -> k (Service vServiceIn cServiceOut) <* closeConnection conn <* sealAll

