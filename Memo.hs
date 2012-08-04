{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Concurrent ( threadDelay )
import Control.Concurrent.MVar
import qualified Control.Exception as CE
import Control.Pipe.C3
import Control.Pipe.Socket
import qualified Data.ByteString.Char8 as B
import Data.Default ( def )
import Data.Serialize ( Serialize )
import qualified Data.Map as M
import GHC.Generics
import qualified Network.Socket as NS
import System.Posix.Daemon

data Command = MemoGet B.ByteString
             | MemoPut B.ByteString B.ByteString
               deriving ( Generic, Show )

instance Serialize Command

data Response = MemoFailed String
              | MemoValue B.ByteString
                deriving ( Generic, Show )

instance Serialize Response

type Book = M.Map B.ByteString B.ByteString

runMemoCommand :: MVar Book -> Command -> IO Response
runMemoCommand bookVar comm = modifyMVar bookVar $ \book -> return $
    case comm of
      MemoGet key -> ( book
                     , maybe (MemoFailed "not found")
                             MemoValue
                             (M.lookup key book) )
      MemoPut key value -> ( M.insert key value book
                           , MemoValue "ok" )

bindPort :: Int -> IO NS.Socket
bindPort port = do
    CE.bracketOnError
        (NS.socket NS.AF_INET NS.Stream NS.defaultProtocol)
        NS.sClose
        (\socket -> do
            NS.setSocketOption socket NS.ReuseAddr 1
            NS.bindSocket socket (NS.SockAddrInet (fromIntegral port)
                                                  NS.iNADDR_ANY)
            NS.listen socket NS.maxListenQueue
            return socket)

startDaemon :: (Serialize a, Serialize b) => Int -> (a -> IO b) -> IO ()
startDaemon port executeCommand = do
    runDetached Nothing def $ do
        CE.bracket
            (bindPort port)
            NS.sClose
            (\lsocket ->
                 runSocketServer lsocket $ commandReceiver executeCommand)

getSocket :: String -> Int -> IO NS.Socket
getSocket hostname port = do
    addrInfos <- NS.getAddrInfo Nothing (Just hostname) (Just $ show port)
    CE.bracketOnError
        (NS.socket NS.AF_INET NS.Stream NS.defaultProtocol)
        NS.sClose
        (\socket -> do
             NS.connect socket (NS.addrAddress $ head addrInfos)
             return socket)

runClient :: (Serialize a, Serialize b) => String -> Int -> a -> IO (Maybe b)
runClient hostname port comm = do
    CE.bracket
        (getSocket hostname port)
        NS.sClose
        (\socket ->
             runSocketClient socket (commandSender comm))

main :: IO ()
main = NS.withSocketsDo $ do
    bookVar <- newMVar M.empty
    startDaemon 7856 (runMemoCommand bookVar)
    threadDelay 1000000
    res <- runClient "localhost"  7856 (MemoPut "name" "alex")
    print (res :: Maybe Response)
