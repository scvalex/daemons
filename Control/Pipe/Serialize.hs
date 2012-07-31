module Control.Pipe.Serialize (
        -- * Pipes
        serializer, deserializer
    ) where

import qualified Data.ByteString.Char8 as B
import Data.Serialize
import Control.Pipe
import Control.Monad ( forever )

deserializer :: (Serialize a, Monad m) => Pipe B.ByteString a m ()
deserializer = loop Nothing Nothing
  where
    loop mk mbin = do
        bin <- maybe await return mbin
        case (maybe (runGetPartial get) id mk) bin of
          Fail reason -> fail reason
          Partial k   -> loop (Just k) Nothing
          Done c bin' -> do
              yield c
              loop Nothing (Just bin')

serializer :: (Serialize a, Monad m) => Pipe a B.ByteString m ()
serializer = forever $ do
    x <- await
    yield (encode x)
