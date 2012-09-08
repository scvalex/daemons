{-# LANGUAGE DeriveGeneric, OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies #-}

module Main where

import Control.Concurrent.MVar ( MVar, newMVar, modifyMVar )
import Data.ByteString.Char8 ( ByteString, unpack )
import Data.Default ( def )
import Data.Serialize ( Serialize )
import Data.String ( fromString )
import qualified Data.Map as M
import GHC.Generics
import System.Environment ( getArgs )
import System.Daemon

data CommandV0 = Register ByteString Port
               | WhereIs ByteString
                 deriving ( Generic, Show )

instance Serialize CommandV0

data Command = CommandV0 CommandV0
               deriving ( Generic, Show )

instance Serialize Command

data ResponseV0 = Ok
                | NotFound ByteString
                | AtPort ByteString Port
                  deriving ( Generic, Show )

instance Serialize ResponseV0

data Response = ResponseV0 ResponseV0
                deriving ( Generic, Show )

instance Serialize Response

type Registry = M.Map ByteString Port

class VersionOf a b | a -> b where
    toLatest :: a -> b
    fromLatest :: b -> a

instance VersionOf Command CommandV0 where
    toLatest (CommandV0 v0) = v0
    fromLatest v0 = CommandV0 v0

instance VersionOf Response ResponseV0 where
    toLatest (ResponseV0 v0) = v0
    fromLatest v0 = ResponseV0 v0

namePort :: Port
namePort = 4370

handleCommand :: MVar Registry -> CommandV0 -> IO ResponseV0
handleCommand registryVar cmd = modifyMVar registryVar $ \registry -> return $
    case cmd of
      WhereIs name -> ( registry
                      , maybe (NotFound name) (AtPort name) (M.lookup name registry) )
      Register name port -> ( M.insert name port registry
                            , Ok )

wrapVersion :: (CommandV0 -> IO ResponseV0) -> Command -> IO Response
wrapVersion f cmd = do
  rsp <- f (toLatest cmd)
  return (fromLatest rsp)

main :: IO ()
main = do
    registryVar <- newMVar M.empty
    let options = def { daemonPort = namePort }
    ensureDaemonRunning "name" options (wrapVersion (handleCommand registryVar))
    args <- getArgs
    let args' = map fromString args
    res <- case args' of
      ["where-is", key] ->
          runClient "localhost" namePort (CommandV0 (WhereIs key))
      ["register", name, port] ->
          let portNum = read (unpack port) in
          runClient "localhost" namePort (CommandV0 (Register name portNum))
      _ ->
          error "invalid command"
    print (fmap toLatest (res :: Maybe Response))
