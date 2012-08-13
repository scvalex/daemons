Memo
====

> A simple in-memory key-value store

Welcome to the first `daemons` tutorial, in which we walk through
writing an in-memory key-value store with an RPC-like interface.  The
code for this tutorial is [Memo.hs](Memo.hs).

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
    
    data Command = Get ByteString
                 | Put ByteString ByteString
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
    main = withSocketsDo $ do
        bookVar <- newMVar M.empty
        let options = def { daemonPort = 7856 }
        startDaemon "memo" options (handleCommand bookVar)
        args <- getArgs
        let args' = map (fromString . map toLower) args
        res <- case args' of
          ["get", key]        -> runClient "localhost"  7856 (Get key)
          ["put", key, value] -> runClient "localhost"  7856 (Put key value)
          _                   -> error "invalid command"
        print (res :: Maybe Response)
