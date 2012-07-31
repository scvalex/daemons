{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import Data.Serialize
import GHC.Generics

data Command = MemoGet B.ByteString
             | MemoPut B.ByteString B.ByteString
               deriving ( Generic, Show )

instance Serialize Command

main :: IO ()
main = do
    let bin = encode (MemoGet "name")
        bin_init = B.init bin
        bin_last = B.singleton (B.last bin)
    case runGetPartial get bin_init :: Result Command of
      Fail reason -> putStrLn ("Error: " ++ reason)
      Done c _    -> print c
      Partial k   -> print (k bin_last)
