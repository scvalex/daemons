module Control.Pipe.C3 (
        -- * Pipes
        commandSender
    ) where

import Control.Pipe ( runPipe, await, yield, (<+<) )
import Control.Pipe.Serialize ( serializer, deserializer )
import Control.Pipe.Socket ( Handler )
import Data.Serialize ( Serialize )

commandSender :: (Serialize a, Serialize b)
              => a
              -> Handler (Maybe b)
commandSender command reader writer = do
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
