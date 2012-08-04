module Control.Pipe.C3 (
        -- * Pipes
        commandSender, commandReceiver
    ) where

import Control.Monad ( forever )
import Control.Monad.Trans.Class ( lift )
import Control.Pipe ( runPipe, await, yield, (<+<) )
import Control.Pipe.Serialize ( serializer, deserializer )
import Control.Pipe.Socket ( Handler )
import Data.Serialize ( Serialize )

commandSender :: (Serialize a, Serialize b) => a -> Handler (Maybe b)
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

commandReceiver :: (Serialize a, Serialize b) => (a -> IO b) -> Handler ()
commandReceiver executeCommand reader writer = do
    runPipe (writer <+< serializer
             <+< commandExecuter
             <+< deserializer <+< reader)
  where
    commandExecuter = forever $ do
      comm <- await
      yield =<< lift (executeCommand comm)
