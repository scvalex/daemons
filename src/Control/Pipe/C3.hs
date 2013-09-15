module Control.Pipe.C3 (
        -- * Pipes
        commandSender, commandReceiver
    ) where

import Control.Monad ( forever )
import Control.Monad.Trans.Class ( lift )
import Control.Pipe.Serialize ( serializer, deserializer )
import Control.Pipe.Socket ( Handler )
import Data.Serialize ( Serialize )
import Pipes ( runEffect, await, yield, (<-<) )

-- | Send a single command over the outgoing pipe and wait for a
-- response.  If the incoming pipe is closed before a response
-- arrives, returns @Nothing@.
commandSender :: (Serialize a, Serialize b) => a -> Handler (Maybe b)
commandSender command reader writer = runEffect $ do
    writer <-< serializer <-< sendCommand
    receiveResponse
        <-< (deserializer >> return Nothing)
        <-< (reader >> return Nothing)
  where
    sendCommand = do
        yield command

    receiveResponse = do
        res <- await
        return (Just res)

-- | Wait for commands on the incoming pipe, handle them, and send the
-- reponses over the outgoing pipe.
commandReceiver :: (Serialize a, Serialize b) => (a -> IO b) -> Handler ()
commandReceiver executeCommand reader writer = runEffect $
    writer <-< serializer <-< commandExecuter <-< deserializer <-< reader
  where
    commandExecuter = forever $ do
        comm <- await
        yield =<< lift (executeCommand comm)
