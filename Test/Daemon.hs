module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Default
import Data.Monoid
import System.Directory
import System.Posix.Daemon

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

main :: IO ()
main = defaultMainWithOpts
       [ testCase "firstRun" testFirst
       ] mempty

ensureRemoved :: FilePath -> IO ()
ensureRemoved filepath = do
    exists <- doesFileExist filepath
    when exists $ do
        removeFile filepath

-- Wait the given number of ms.
sleep :: Int -> IO ()
sleep n = threadDelay (n * 1000)

testFirst :: Assertion
testFirst = flip finally (ensureRemoved "tmp") $ do
    let txtExp = "42"
    runDetached Nothing def $ do
        writeFile "tmp" txtExp
    sleep 500
    txt <- readFile "tmp"
    txt @?= txtExp
