module Control.Pipe.C3 (
        -- * Pipes
        commandSender
    ) where

import Data.ByteString.Char8 ( ByteString )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Pipe ( Producer, Consumer
                    , runPipe, await, yield, (<+<) )
import Control.Pipe.Serialize ( serializer, deserializer )
import Data.Serialize ( Serialize )

commandSender :: (Serialize a, Serialize b, MonadIO m)
              => a
              -> Producer ByteString IO ()
              -> Consumer ByteString IO ()
              -> m (Maybe b)
commandSender command reader writer = liftIO $ do
    runPipe (writer <+< serializer <+< sendCommand)
    runPipe (receiveResponse
             <+< (deserializer >> return Nothing)
             <+< (reader >> return Nothing))
  where
    sendCommand = do
        yield command

    receiveResponse = do
        res <- await
        return (Just res)
