module System.Daemon (
        -- * Daemons and clients
        startDaemon, runClient
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

bindPort :: Int -> IO Socket
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

startDaemon :: (Serialize a, Serialize b) => String -> Int -> (a -> IO b) -> IO ()
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

getSocket :: String -> Int -> IO Socket
getSocket hostname port = do
    addrInfos <- getAddrInfo Nothing (Just hostname) (Just $ show port)
    CE.bracketOnError
        (socket AF_INET Stream defaultProtocol)
        sClose
        (\s -> do
             connect s (addrAddress $ head addrInfos)
             return s)

runClient :: (Serialize a, Serialize b) => String -> Int -> a -> IO (Maybe b)
runClient hostname port comm = do
    CE.bracket
        (getSocket hostname port)
        sClose
        (\s ->
             runSocketClient s (commandSender comm))
