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
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as B
import Network.Socket ( Socket )
import qualified Network.Socket as NS
import Network.Socket.ByteString ( sendAll, recv )
import Pipes ( Consumer, Producer, await, yield )

-- | Stream data from the socket.
socketReader :: (MonadIO m) => Socket -> Producer ByteString m ()
socketReader socket = do
    bin <- lift . liftIO $ recv socket 4096
    unless (B.null bin) $ do
        yield bin
        socketReader socket

-- | Stream data to the socket.
socketWriter :: (MonadIO m) => Socket -> Consumer ByteString m ()
socketWriter socket = forever $ do
    bin <- await
    lift . liftIO $ sendAll socket bin

-- | A simple handler: takes an incoming stream of 'ByteString's, an
-- stream of 'ByteString's, and ties them together somehow.
-- Conceptually, the simplest handler would be @identity@:
--
-- > import Control.Monad
-- > import Control.Pipe
-- > import Data.ByteString.Char8
-- >
-- > handler reader writer = do
-- >     let identity = forever $ do
-- >         x <- await
-- >         yield x
-- >     runPipe (writer <+< identity <+< reader)
--
-- See the @pipes@ tutorial for more examples of writing pipes.
--
-- Since 'ByteString's are fairly boring by themseleves, have a look
-- at "Control.Pipe.Serialize" which lets you deserialize/serialize
-- pipes of 'ByteString's easily.
type Handler r = Producer ByteString IO ()
              -> Consumer ByteString IO ()
              -> IO r

-- | Listen for connections on the given socket, and run 'Handler' on
-- each received connection.  The socket should previously have been
-- bound to a port or to a file.  Each handler is run in its own
-- thread.  Even in case of an error, the handlers' sockets are
-- closed.
runSocketServer :: (MonadIO m) => Socket -> Handler () -> m ()
runSocketServer lsocket handler = liftIO $ forever $ do
    (socket, _addr) <- NS.accept lsocket
    _ <- forkIO $ CE.finally
                      (handler (socketReader socket) (socketWriter socket))
                      (NS.close socket)
    return ()

-- | Run 'Handler' on the given socket.
runSocketClient :: (MonadIO m) => Socket -> Handler r -> m r
runSocketClient socket handler = liftIO $ do
    handler (socketReader socket) (socketWriter socket)
