module System.Daemon (
        -- * Daemons and clients
        startDaemon, runClient,

        -- * Types
        HostName, Port
    ) where

import Control.Concurrent ( threadDelay )
import qualified Control.Exception as CE
import Control.Monad ( when )
import Control.Pipe.C3 ( commandSender, commandReceiver )
import Control.Pipe.Socket ( runSocketServer, runSocketClient )
import Data.Default ( def )
import Data.Serialize ( Serialize )
import Network.Socket ( Socket, SockAddr(..), Family(..), SocketType(..)
                      , SocketOption(..), setSocketOption
                      , socket, sClose, connect, bindSocket, listen
                      , getAddrInfo, addrAddress
                      , defaultProtocol, iNADDR_ANY, maxListenQueue )
import System.Directory ( getHomeDirectory, doesFileExist )
import System.FilePath ( (</>), (<.>) )
import System.Posix.Daemon ( runDetached )

type Port = Int
type HostName = String

-- | Start a daemon running on the given port, using the given handler
-- to respond to events.  If the daemon is already running, just
-- return.
--
-- The pidfile @~/.name.pid@ will be created and locked.  This
-- function checks the pidfile to see if the daemon is already
-- running.
--
-- The daemon will listen for incoming connections on all interfaces
-- on @port@.
--
-- The @handler@ is just a function that takes a command and returns a
-- response.
startDaemon :: (Serialize a, Serialize b)
            => String       -- ^ name
            -> Port         -- ^ port
            -> (a -> IO b)  -- ^ handler
            -> IO ()
startDaemon name port executeCommand = do
    home <- getHomeDirectory
    let pidfile = home </> ("." ++ name) <.> "pid"
    dfe <- doesFileExist pidfile
    when (not dfe) $ do
        runDetached (Just pidfile) def $ do
            CE.bracket
                (bindPort port)
                sClose
                (\lsocket ->
                     runSocketServer lsocket $ commandReceiver executeCommand)
        threadDelay 1000000

-- | Send a command to the daemon running at the given network address
-- and wait for a response.
--
-- If the connection is closed before receiving a response, return
-- 'Nothing'.
runClient :: (Serialize a, Serialize b)
          => HostName  -- ^ hostname
          -> Port      -- ^ port
          -> a         -- ^ command
          -> IO (Maybe b)
runClient hostname port comm = do
    CE.bracket
        (getSocket hostname port)
        sClose
        (\s ->
             runSocketClient s (commandSender comm))

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
