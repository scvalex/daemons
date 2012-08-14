{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import Control.Concurrent.MVar ( MVar, newMVar, modifyMVar )
import Data.ByteString.Char8 ( ByteString )
import Data.Default ( def )
import Data.Serialize ( Serialize )
import Data.String ( fromString )
import qualified Data.Map as M
import GHC.Generics
import System.Environment ( getArgs )
import System.Daemon

data Command = Put ByteString ByteString
             | Get ByteString
               deriving ( Generic, Show )

instance Serialize Command

data Response = Failed String
              | Value ByteString
                deriving ( Generic, Show )

instance Serialize Response

type Book = M.Map ByteString ByteString

handleCommand :: MVar Book -> Command -> IO Response
handleCommand bookVar comm = modifyMVar bookVar $ \book -> return $
    case comm of
      Get key -> ( book
                 , maybe (Failed "not found") Value (M.lookup key book) )
      Put key value -> ( M.insert key value book
                       , Value "ok" )

main :: IO ()
main = do
    bookVar <- newMVar M.empty
    let options = def { daemonPort = 7856 }
    startDaemon "memo" options (handleCommand bookVar)
    args <- getArgs
    let args' = map fromString args
    res <- case args' of
      ["get", key]        -> runClient "localhost"  7856 (Get key)
      ["put", key, value] -> runClient "localhost"  7856 (Put key value)
      _                   -> error "invalid command"
    print (res :: Maybe Response)

{-
% dist/build/memo/memo get apples
Just (Failed "not found")

% dist/build/memo/memo put apples 23
Just (Value "ok")

% dist/build/memo/memo get apples
Just (Value "23")
-}
