{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import Data.Serialize
import Data.String
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

memoGenerator :: Int -> Producer Command IO ()
memoGenerator n = replicateM_ n $ do
    m <- lift $ randomRIO (1, n)
    yield (MemoGet (fromString (show m)))

printer :: Consumer Command IO ()
printer = forever $ do
    x <- await
    lift $ print x

testWrite :: Pipeline IO ()
testWrite = fileWriter "test.bin" <+< serializer <+< memoGenerator 10

testRead :: Pipeline IO ()
testRead = printer <+< deserializer <+< fileReader "test.bin"

main :: IO ()
main = do
    runPipe testWrite
    runPipe testRead
