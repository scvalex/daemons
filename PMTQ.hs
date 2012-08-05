{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import Control.Concurrent.Chan ( Chan, newChan, readChan, writeChan )
import Control.Concurrent.MVar ( MVar, newMVar, modifyMVar )
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as B
import Data.Char ( toLower )
import Data.Default ( def )
import Data.Serialize ( Serialize )
import Data.String ( fromString )
import qualified Data.Map as M
import GHC.Generics
import Network.Socket ( withSocketsDo )
import System.Environment ( getArgs )
import System.Daemon
import System.IO ( hPutStrLn, stderr )

data Command = Push ByteString ByteString
             | Pop ByteString
             | Consume ByteString
               deriving ( Generic, Show )

instance Serialize Command

data Response = Value ByteString
                deriving ( Generic, Show )

instance Serialize Response

type Registry = M.Map ByteString (Chan ByteString)

handleCommand :: MVar Registry -> Command -> IO Response
handleCommand registryVar (Pop topic) = do
    ch <- getCreateChan registryVar topic
    val <- readChan ch
    return (Value val)
handleCommand registryVar (Push topic val) = do
    ch <- getCreateChan registryVar topic
    writeChan ch val
    return (Value "ok")

-- Get the channel for the given topic, and create it if it does not
-- already exist.
getCreateChan :: MVar Registry -> ByteString -> IO (Chan ByteString)
getCreateChan registryVar topic = modifyMVar registryVar $ \registry -> do
    case M.lookup topic registry of
      Nothing -> do
          ch <- newChan
          return (M.insert topic ch registry, ch)
      Just ch -> do
          return (registry, ch)

printResult :: Maybe Response -> IO ()
printResult Nothing            = hPutStrLn stderr "no response"
printResult (Just (Value val)) = B.putStrLn val

main :: IO ()
main = withSocketsDo $ do
    registryVar <- newMVar M.empty
    let options = def { daemonPort = 7857 }
    startDaemon "pmtq" options (handleCommand registryVar)
    args <- getArgs
    let args' = map (fromString . map toLower) args
    res <- case args' of
      ["pop", key]         -> runClient "localhost" 7857 (Pop key)
      ["push", key, value] -> runClient "localhost" 7857 (Push key value)
      ["consume", key]     -> runClient "localhost" 7857 (Consume key)
      _                    -> error "invalid command"
    printResult res
