{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import Control.Concurrent.MVar
import Control.Pipe
import Control.Pipe.Serialize
import Control.Pipe.Socket
import Control.Monad
import Control.Monad.Trans.Class
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

commandExecuter :: MVar Book -> Pipe Command Result IO ()
commandExecuter bookVar = forever $ do
    comm <- await
    yield =<< lift (executeCommand bookVar comm)

main :: IO ()
main = do
    bookVar <- newMVar M.empty
    let lsocket = undefined
    runDetached Nothing def $ runSocketServer lsocket $ \reader writer -> do
           runPipe (writer <+< serializer
                    <+< commandExecuter bookVar
                    <+< deserializer <+< reader)
    let socket = undefined
    runSocketClient socket $ \_reader writer ->
        runPipe (writer <+< serializer <+< (yield (MemoPut "name" "alex")))
