-- | An RPC-like interface for daemons is provided by
-- 'ensureDaemonRunning' and 'runClient'.
--
-- A more versatile interface that lets you supply your own `Handler`
-- is provided by `ensureDaemonWithHandlerRunning` and
-- `runClientWithHandler`.  These are useful if, for instance, you
-- need streaming requests or replies, or if you need to change your
-- event handler at runtime.
--
-- The event handling loop is provided by `runInForeground`.  You may
-- want to use this for debugging purposes or if you want to handle
-- daemonization manually.
module System.Daemon (
        -- * Daemons
        ensureDaemonRunning, ensureDaemonWithHandlerRunning,

        -- * Clients,
        runClient, runClientWithHandler,

        -- * Types
        DaemonOptions(..), PidFile(..), HostName, Port,

        -- * Helpers
        runInForeground, bindPort, getSocket
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
                      , socket, close, connect, bind, listen
                      , AddrInfo(..), getAddrInfo, addrAddress, defaultHints
                      , defaultProtocol, tupleToHostAddress, maxListenQueue )
import System.Directory ( getHomeDirectory )
import System.FilePath ( (</>), (<.>) )
import System.Posix.Daemon ( runDetached, isRunning )
import Text.Printf ( printf )

type Port = Int
type HostName = String

-- | The configuration options of a daemon.  See 'ensureDaemonRunning'
-- for a description of each.
data DaemonOptions = DaemonOptions
    { daemonPort           :: Port
    , daemonPidFile        :: PidFile
    , printOnDaemonStarted :: Bool
    } deriving ( Show )

instance Default DaemonOptions where
    def = DaemonOptions { daemonPort           = 5000
                        , daemonPidFile        = InHome
                        , printOnDaemonStarted = True
                        }

-- | The location of the daemon's pidfile.
data PidFile = InHome
             | PidFile FilePath
               deriving ( Show )

instance IsString PidFile where
    fromString = PidFile

-- | Simple wrapper around 'ensureDaemonWithHandlerRunning' which uses
-- a simple function to respond to commands and doesn't deal with
-- pipes.
--
-- The @handler@ is just a function that takes a command and returns a
-- response.
ensureDaemonRunning :: (Serialize a, Serialize b)
                    => String         -- ^ name
                    -> DaemonOptions  -- ^ options
                    -> (a -> IO b)    -- ^ handler
                    -> IO ()
ensureDaemonRunning name options executeCommand = do
    ensureDaemonWithHandlerRunning name options (commandReceiver executeCommand)

-- FIXME Add set-up and tear-down action.  The reason the threaded
-- runtime wouldn't work was because we were creating the mvar in a
-- different thread!

-- | Start a daemon running on the given port, using the given handler
-- to respond to events.  If the daemon is already running, don't do
-- anything.  Returns immediately.
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
ensureDaemonWithHandlerRunning :: String         -- ^ name
                               -> DaemonOptions  -- ^ options
                               -> Handler ()     -- ^ handler
                               -> IO ()
ensureDaemonWithHandlerRunning name options handler = do
    home <- getHomeDirectory
    let pidfile = case daemonPidFile options of
                    InHome       -> home </> ("." ++ name) <.> "pid"
                    PidFile path -> path
    running <- isRunning pidfile
    when (not running) $ do
        runDetached (Just pidfile) def
            (runInForeground (daemonPort options) handler)
        when (printOnDaemonStarted options)
            (printf "Daemon started on port %d\n" (daemonPort options))
        threadDelay (1 * 1000 * 1000)  -- 1s delay

-- | Start the given handler in the foreground.  It will listen and
-- respond to events on the given port.
--
-- This is the function that 'ensureDaemonWithHandlerRunning' runs on
-- the daemon thread.
runInForeground :: Port -> Handler () -> IO ()
runInForeground port handler = do
    CE.bracket
        (bindPort port)
        close
        (\lsocket ->
             runSocketServer lsocket handler)

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
        close
        (\s -> runSocketClient s handler)

-- | Create a socket and bind it to the given port.
bindPort :: Port -> IO Socket
bindPort port = do
    CE.bracketOnError
        (socket AF_INET Stream defaultProtocol)
        close
        (\s -> do
            -- FIXME See the examples at the end of Network.Socket.ByteString
            setSocketOption s ReuseAddr 1
            bind s (SockAddrInet (fromIntegral port) (tupleToHostAddress (0, 0, 0, 0)))
            listen s maxListenQueue
            return s)

-- | Create a socket connected to the given network address.
getSocket :: HostName -> Port -> IO Socket
getSocket hostname port = do
    addrInfos <- getAddrInfo (Just (defaultHints { addrFamily = AF_INET }))
                             (Just hostname)
                             (Just $ show port)
    CE.bracketOnError
        (socket AF_INET Stream defaultProtocol)
        close
        (\s -> do
             connect s (addrAddress $ head addrInfos)
             return s)
