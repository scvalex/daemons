{-# LANGUAGE ScopedTypeVariables #-}

-- Thank you:
--   <a href="https://github.com/pcapriotti/pipes-network">pipes-network</a>

module Control.Pipe.Socket (
        -- * Socket pipes
        socketReader, socketWriter,

        -- * Socket server/client
        Handler, runSocketServer, runSocketClient
    ) where

import Control.Concurrent ( forkIO )
import qualified Control.Exception as CE
import Control.Monad ( forever, unless )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Class ( lift )
import Control.Pipe ( Pipe, Consumer, Producer
                    , await, yield, runPipe, (<+<) )
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as B
import Network.Socket ( Socket )
import qualified Network.Socket as NS
import Network.Socket.ByteString ( sendAll, recv )

-- | Stream data from the socket.
socketReader :: (MonadIO m) => Socket -> Producer ByteString m ()
socketReader socket = do
    bin <- lift . liftIO $ recv socket 4096
    unless (B.null bin) $ do
        yield bin
        socketReader socket

-- | Stream data to the socket.
socketWriter :: (MonadIO m) => Socket -> Consumer ByteString m r
socketWriter socket = forever $ do
    bin <- await
    lift . liftIO $ sendAll socket bin

-- | A simple handler: takes an incoming stream of 'ByteString's and
-- ouputs a stream of 'ByteString's.  Since 'ByteString's are fairly
-- boring by themseleves, have a look at "Control.Pipe.Serialize"
-- which lets you deserialize/serialize pipes of 'ByteString's easily.
type Handler = Pipe ByteString ByteString IO ()

-- | Listen for connections on the given socket, and run 'Handler' on
-- each received connection.  The socket should previously have been
-- bound to a port or to a file.  Each handler is run in its own
-- thread.  The socket is closed, even in the case of errors.
runSocketServer :: (MonadIO m) => Socket -> Handler -> m ()
runSocketServer lsocket handler = liftIO $
    CE.finally serve (NS.sClose lsocket)
  where
    serve = forever $ do
        (socket, _addr) <- NS.accept lsocket
        let pipeline = socketWriter socket <+< handler <+< socketReader socket
        _ <- forkIO $ CE.finally
                          (runPipe pipeline)
                          (NS.sClose socket)
        return ()

-- | Run 'Handler' on the given socket.  The socket is closed, even in
-- the case of errors.
runSocketClient :: (MonadIO m) => Socket -> Handler -> m ()
runSocketClient socket handler = liftIO $ do
    let pipeline = socketWriter socket <+< handler <+< socketReader socket
    CE.finally (runPipe pipeline) (NS.sClose socket)
