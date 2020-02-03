-- | This module provides the 'deserializer' and 'serializer' pipes to
-- convert 'B.ByteString's off of pipes into typed values.
--
-- In order to use it, the types of the values need to have
-- 'Serialize' instances.  These can be derived automatically using
-- "Ghc.Generics":
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > data Foo = Bar String | Baz Int
-- >            deriving ( Generic )
-- >
-- > instance Serialize Foo
--
-- Note that in the above example: we use the @DeriveGeneric@
-- extension, derive a @Generic@ instance for our data-type, and write
-- an /empty/ @Serialize@ instance.
--
module Control.Pipe.Serialize (
        -- * Pipes
        serializer, deserializer
    ) where

import Data.ByteString.Char8 ( ByteString )
import Data.Serialize ( Serialize, get, encode
                      , Result(..), runGetPartial )
import Pipes ( Pipe, await, yield )
import Control.Monad ( forever )
import Control.Monad.Fail ( MonadFail )

-- | De-serialize data from strict 'ByteString's.  Uses @cereal@'s
-- incremental 'Data.Serialize.Get' parser.
deserializer :: (Serialize a, Monad m, MonadFail m) => Pipe ByteString a m ()
deserializer = loop Nothing Nothing
  where
    loop mk mbin = do
        bin <- maybe await return mbin
        case (maybe (runGetPartial get) id mk) bin of
          Fail reason _leftover ->
              fail reason
          Partial k ->
              loop (Just k) Nothing
          Done c bin' -> do
              yield c
              loop Nothing (Just bin')

-- | Serialize data into strict 'ByteString's.
serializer :: (Serialize a, Monad m) => Pipe a ByteString m ()
serializer = forever $ do
    x <- await
    yield (encode x)
