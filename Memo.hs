{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import Control.Concurrent.MVar
import Control.Pipe.C3
import Control.Pipe.Socket
import qualified Data.ByteString.Char8 as B
import Data.Default ( def )
import Data.Serialize ( Serialize )
import qualified Data.Map as M
import GHC.Generics
import System.Posix.Daemon

data Command = MemoGet B.ByteString
             | MemoPut B.ByteString B.ByteString
               deriving ( Generic, Show )

instance Serialize Command

data Result = MemoFailed String
            | MemoValue B.ByteString
              deriving ( Generic, Show )

instance Serialize Result

type Book = M.Map B.ByteString B.ByteString

executeCommand :: MVar Book -> Command -> IO Result
executeCommand bookVar comm = modifyMVar bookVar $ \book -> return $
    case comm of
      MemoGet key -> ( book
                     , maybe (MemoFailed "not found")
                             MemoValue
                             (M.lookup key book) )
      MemoPut key value -> ( M.insert key value book
                           , MemoValue "ok" )

main :: IO ()
main = do
    bookVar <- newMVar M.empty
    let lsocket = undefined
    runDetached Nothing def $ do
        runSocketServer lsocket $ commandReceiver (executeCommand bookVar)
    let socket = undefined
    res <- runSocketClient socket $ commandSender (MemoPut "name" "alex")
    print (res :: Maybe Result)
