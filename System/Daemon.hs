module System.Daemon (
        -- * Daemons
        startDaemon, startDaemonWithHandler,

        -- * Clients,
        runClient, runClientWithHandler,

        -- * Types
        DaemonOptions(..), PidFile(..), HostName, Port,

        -- * Helpers
        bindPort, getSocket
    ) where

import Control.Concurrent ( threadDelay )
import qualified Control.Exception as CE
import Control.Monad ( when )
import Control.Pipe.C3 ( commandSender, commandReceiver )
import Control.Pipe.Socket ( Handler, runSocketServer, runSocketClient )
import Data.Default ( Default(..) )
import Data.Serialize ( Serialize )
import Data.String ( IsString(..) )
import Network.Socket ( Socket, SockAddr(..), Family(..), SocketType(..)
                      , SocketOption(..), setSocketOption
                      , socket, sClose, connect, bindSocket, listen
                      , getAddrInfo, addrAddress
                      , defaultProtocol, iNADDR_ANY, maxListenQueue )
import System.Directory ( getHomeDirectory )
import System.FilePath ( (</>), (<.>) )
import System.Posix.Daemon ( runDetached, isRunning )

type Port = Int
type HostName = String

-- | The configuration options of a daemon.  See 'startDaemon' for a
-- description of each.
data DaemonOptions = DaemonOptions
    { daemonPort     :: Port
    , daemonPidFile  :: PidFile
    } deriving ( Show )

instance Default DaemonOptions where
    def = DaemonOptions { daemonPort    = 5000
                        , daemonPidFile = InHome
                        }

-- | The location of the daemon's pidfile.
data PidFile = InHome
             | PidFile FilePath
               deriving ( Show )

instance IsString PidFile where
    fromString = PidFile

-- | Simple wrapper around 'startDaemonWithHandler' which uses a
-- simple function to respond to commands and doesn't deal with pipes.
--
-- The @handler@ is just a function that takes a command and returns a
-- response.
startDaemon :: (Serialize a, Serialize b)
            => String         -- ^ name
            -> DaemonOptions  -- ^ options
            -> (a -> IO b)    -- ^ handler
            -> IO ()
startDaemon name options executeCommand = do
    startDaemonWithHandler name options (commandReceiver executeCommand)

-- | Start a daemon running on the given port, using the given handler
-- to respond to events.  If the daemon is already running, just
-- return.
--
-- The pidfile @PidFile options@ will be created and locked.  This
-- function checks the pidfile to see if the daemon is already
-- running.
--
-- The daemon will listen for incoming connections on all interfaces
-- on @daemonPort options@.
--
-- The @handler@ is a function that takes the reader and writer
-- 'ByteString' pipes and does something with them.  See
-- 'commandReceiver' for an example handler.
startDaemonWithHandler :: String         -- ^ name
                       -> DaemonOptions  -- ^ options
                       -> Handler ()     -- ^ handler
                       -> IO ()
startDaemonWithHandler name options handler = do
    home <- getHomeDirectory
    let pidfile = case daemonPidFile options of
                    InHome       -> home </> ("." ++ name) <.> "pid"
                    PidFile path -> path
    running <- isRunning pidfile
    when (not running) $ do
        runDetached (Just pidfile) def $ do
            CE.bracket
                (bindPort (daemonPort options))
                sClose
                (\lsocket ->
                     runSocketServer lsocket handler)
        threadDelay 1000000

-- | Send a command to the daemon running at the given network address
-- and wait for a response.
--
-- This is a simple wrapper around 'runClientWithHandler' that sends a
-- single command and waits for a single response.
--
-- If the connection is closed before receiving a response, return
-- 'Nothing'.
runClient :: (Serialize a, Serialize b)
          => HostName  -- ^ hostname
          -> Port      -- ^ port
          -> a         -- ^ command
          -> IO (Maybe b)
runClient hostname port comm =
    runClientWithHandler hostname port (commandSender comm)

-- | Connect to the given network address and run the handler on the
-- reader and wrier pipes for the socket.
--
-- The @handler@ is a function that takes the reader and writer
-- 'ByteString' pipes and does something with them.  For an example
-- handler, see 'commandSender', which sends a command and waits for a
-- response.
runClientWithHandler :: HostName   -- ^ hostname
                     -> Port       -- ^ port
                     -> Handler a  -- ^ command
                     -> IO a
runClientWithHandler hostname port handler = do
    CE.bracket
        (getSocket hostname port)
        sClose
        (\s -> runSocketClient s handler)

-- | Create a socket and bind it to the given port.
bindPort :: Port -> IO Socket
bindPort port = do
    CE.bracketOnError
        (socket AF_INET Stream defaultProtocol)
        sClose
        (\s -> do
            setSocketOption s ReuseAddr 1
            bindSocket s (SockAddrInet (fromIntegral port)
                                                  iNADDR_ANY)
            listen s maxListenQueue
            return s)

-- | Create a socket connected to the given network address.
getSocket :: HostName -> Port -> IO Socket
getSocket hostname port = do
    addrInfos <- getAddrInfo Nothing (Just hostname) (Just $ show port)
    CE.bracketOnError
        (socket AF_INET Stream defaultProtocol)
        sClose
        (\s -> do
             connect s (addrAddress $ head addrInfos)
             return s)
