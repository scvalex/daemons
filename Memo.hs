{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import Data.Serialize ( Serialize )
import Data.String
import qualified Data.Map as M
import Control.Concurrent.MVar
import Control.Pipe
import Control.Pipe.Binary
import Control.Pipe.Serialize
import Control.Monad
import Control.Monad.Trans.Class
import GHC.Generics
import System.Random

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

memoGenerator :: Int -> Producer Command IO ()
memoGenerator n = replicateM_ n $ do
    m <- lift $ randomRIO (1 :: Int, 4)
    b <- lift $ randomRIO (1 :: Int, 2)
    yield (if b == 1 then MemoGet (fromString (show m))
                     else MemoPut (fromString (show m)) "data")

printer :: (Show a) => Consumer a IO ()
printer = forever $ do
    x <- await
    lift $ print x

testWrite :: Pipeline IO ()
testWrite = fileWriter "commands.bin" <+< serializer <+< memoGenerator 10

testRead :: Pipeline IO ()
testRead = (printer :: Consumer Result IO ())
         <+< deserializer
         <+< fileReader "results.bin"

test :: MVar Book -> Pipeline IO ()
test bookVar = fileWriter "results.bin"
             <+< serializer
             <+< commandExecuter bookVar
             <+< deserializer
             <+< fileReader "commands.bin"

main :: IO ()
main = do
    bookVar <- newMVar M.empty
    runPipe testWrite
    runPipe (test bookVar)
    runPipe testRead
