{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import Control.Concurrent.Chan ( Chan, newChan, readChan, writeChan )
import Control.Concurrent.MVar ( MVar, newMVar, modifyMVar )
import Control.Monad ( forever )
import Control.Monad.Trans.Class ( lift )
import Control.Pipe.Serialize ( serializer, deserializer )
import Control.Pipe.Socket ( Handler )
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as B
import Data.Char ( toLower )
import Data.Default ( def )
import Data.Serialize ( Serialize )
import Data.String ( fromString )
import qualified Data.Map as M
import GHC.Generics
import Network.Socket ( withSocketsDo )
import Pipes ( runEffect, (<-<), await, yield )
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

handleCommands :: MVar Registry -> Handler ()
handleCommands registryVar reader writer = runEffect $
    writer <-< serializer <-< commandExecuter <-< deserializer <-< reader
  where
    commandExecuter = forever $ do
        comm <- await
        case comm of
          Pop topic -> do
              ch <- lift $ getCreateChan topic
              transferToPipeFromChan ch
          Consume topic -> do
              ch <- lift $ getCreateChan topic
              forever $ transferToPipeFromChan ch
          Push topic val -> do
              ch <- lift $ getCreateChan topic
              lift $ writeChan ch val
              yield (Value "ok")

    -- Transfer a value from the given channel to the pipe.
    transferToPipeFromChan ch = do
        val <- lift $ readChan ch
        yield (Value val)

    -- Get the channel for the given topic, and create it if it does
    -- not already exist.
    getCreateChan topic = modifyMVar registryVar $ \registry -> do
        case M.lookup topic registry of
          Nothing -> do
              ch <- newChan
              return (M.insert topic ch registry, ch)
          Just ch -> do
              return (registry, ch)

main :: IO ()
main = withSocketsDo $ do
    registryVar <- newMVar M.empty
    let options = def { daemonPort = 7857 }
    ensureDaemonWithHandlerRunning "queue" options (handleCommands registryVar)
    args <- getArgs
    let args' = map (fromString . map toLower) args
    case args' of
      ["pop", key] -> do
          res <- runClient "localhost" 7857 (Pop key)
          printResult res
      ["push", key, value] -> do
          res <- runClient "localhost" 7857 (Push key value)
          printResult res
      ["consume", key] -> do
          runClientWithHandler "localhost" 7857 $ \reader writer ->
              runEffect $ do
                  writer <-< serializer <-< yield (Consume key)
                  (forever $ await >>= \res -> lift (printResult (Just res)))
                       <-< deserializer <-< reader
      _ -> do
          error "invalid command"
  where
    printResult :: Maybe Response -> IO ()
    printResult Nothing            = hPutStrLn stderr "no response"
    printResult (Just (Value val)) = B.putStrLn val
