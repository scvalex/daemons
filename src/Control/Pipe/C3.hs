module Control.Pipe.C3 (
        -- * Pipes
        commandSender, commandSenderByPipe, commandReceiver, commandReceiverByPipe
    ) where

import           Control.Monad             (forever)
import           Control.Monad.Trans.Class (lift)
import           Control.Pipe.Serialize    (deserializer, serializer)
import           Control.Pipe.Socket       (Handler)
import           Data.Serialize            (Serialize)
import           Pipes                     (Consumer, Pipe, await, runEffect,
                                            yield, (<-<), (>~))

-- Like commandSenderByPipe but with only one possible response
commandSender :: (Serialize a, Serialize b) => a -> Handler (Maybe b)
commandSender command = commandSenderByPipe command receiveResponse
  where
    receiveResponse = do
        res <- await
        return res

-- | Send a single command over the outgoing pipe and consume the response with the given consumer
-- If the incoming pipe is closed before a response
-- arrives, returns @Nothing@.
commandSenderByPipe :: (Serialize a, Serialize b) => a -> Consumer b IO b -> Handler (Maybe b)
commandSenderByPipe command consumer reader writer = runEffect $ do
    writer <-< serializer <-< sendCommand
    (consumer >~ nothingFilter)
        <-< (deserializer >> return Nothing)
        <-< (reader >> return Nothing)
  where
    sendCommand = do
        yield command

    nothingFilter = do
      resp <- await
      return $ Just resp


-- | Like commandRecieverByPipe but you supply a single handle function instead of a pipe
commandReceiver :: (Serialize a, Serialize b) => (a -> IO b) -> Handler ()
commandReceiver executeCommand = commandReceiverByPipe commandExecuter
  where
    commandExecuter = forever $ do
        comm <- await
        yield =<< lift (executeCommand comm)

-- | Wait for commands on the incoming pipe, handle them, and send the
-- reponses over the outgoing pipe.
commandReceiverByPipe :: (Serialize a, Serialize b) => Pipe a b IO () -> Handler ()
commandReceiverByPipe executorPipe reader writer = runEffect $
    writer <-< serializer <-< executorPipe <-< deserializer <-< reader

