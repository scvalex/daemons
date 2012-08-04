{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import Control.Concurrent.MVar
import qualified Data.ByteString.Char8 as B
import Data.Char ( toLower )
import Data.Serialize ( Serialize )
import Data.String ( fromString )
import qualified Data.Map as M
import GHC.Generics
import qualified Network.Socket as NS
import System.Environment ( getArgs )
import System.Daemon

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

main :: IO ()
main = NS.withSocketsDo $ do
    bookVar <- newMVar M.empty
    startDaemon "memo" 7856 (runMemoCommand bookVar)

    args <- getArgs
    let args' = map (fromString . map toLower) args
    res <- case args' of
      ["get", key]        -> runClient "localhost"  7856 (MemoGet key)
      ["put", key, value] -> runClient "localhost"  7856 (MemoPut key value)
      _                   -> error "invalid command"
    print (res :: Maybe Response)
