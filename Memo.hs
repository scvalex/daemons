{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import Control.Concurrent.MVar ( MVar, newMVar, modifyMVar )
import Data.ByteString.Char8 ( ByteString )
import Data.Char ( toLower )
import Data.Default ( def )
import Data.Serialize ( Serialize )
import Data.String ( fromString )
import qualified Data.Map as M
import GHC.Generics
import Network.Socket ( withSocketsDo )
import System.Environment ( getArgs )
import System.Daemon

data Command = MemoGet ByteString
             | MemoPut ByteString ByteString
               deriving ( Generic, Show )

instance Serialize Command

data Response = MemoFailed String
              | MemoValue ByteString
                deriving ( Generic, Show )

instance Serialize Response

type Book = M.Map ByteString ByteString

runMemoCommand :: MVar Book -> Command -> IO Response
runMemoCommand bookVar comm = modifyMVar bookVar $ \book -> return $
    case comm of
      MemoGet key -> ( book
                     , maybe (MemoFailed "not found")
                             MemoValue
                             (M.lookup key book) )
      MemoPut key value -> ( M.insert key value book
                           , MemoValue "ok" )

main :: IO ()
main = withSocketsDo $ do
    bookVar <- newMVar M.empty
    let options = def { daemonPort = 7856 }
    startDaemon "memo" options (runMemoCommand bookVar)
    args <- getArgs
    let args' = map (fromString . map toLower) args
    res <- case args' of
      ["get", key]        -> runClient "localhost"  7856 (MemoGet key)
      ["put", key, value] -> runClient "localhost"  7856 (MemoPut key value)
      _                   -> error "invalid command"
    print (res :: Maybe Response)
