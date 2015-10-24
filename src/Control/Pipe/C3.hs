module Control.Pipe.C3 (
        -- * Pipes
        commandSender, commandReceiver, commandRecieverByPipe
    ) where

import           Control.Monad             (forever)
import           Control.Monad.Trans.Class (lift)
import           Control.Pipe.Serialize    (deserializer, serializer)
import           Control.Pipe.Socket       (Handler)
import           Data.Serialize            (Serialize)
import           Pipes                     (Pipe, await, runEffect, yield,
                                            (<-<))

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

-- | Like commandRecieverByPipe but you supply a single handle function instead of a pipe
commandReceiver :: (Serialize a, Serialize b) => (a -> IO b) -> Handler ()
commandReceiver executeCommand = commandRecieverByPipe commandExecuter
  where
    commandExecuter = forever $ do
        comm <- await
        yield =<< lift (executeCommand comm)

-- | Wait for commands on the incoming pipe, handle them, and send the
-- reponses over the outgoing pipe.
commandRecieverByPipe :: (Serialize a, Serialize b) => Pipe a b IO () -> Handler ()
commandRecieverByPipe executorPipe reader writer = runEffect $
    writer <-< serializer <-< executorPipe <-< deserializer <-< reader

