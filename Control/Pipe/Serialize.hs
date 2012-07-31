module Control.Pipe.Serialize (
        -- * Pipes
        serializer, deserializer
    ) where

import Data.ByteString.Char8 ( ByteString )
import Data.Serialize ( Serialize, get, encode
                      , Result(..), runGetPartial )
import Control.Pipe ( Pipe, await, yield )
import Control.Monad ( forever )

-- | De-serialize data from strict 'ByteString's.  Uses @cereal@'s
-- incremental 'Data.Serialize.Get' parser.
deserializer :: (Serialize a, Monad m) => Pipe ByteString a m ()
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

-- | Serialize data into strict 'ByteString's.
serializer :: (Serialize a, Monad m) => Pipe a ByteString m ()
serializer = forever $ do
    x <- await
    yield (encode x)
