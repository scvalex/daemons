{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

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
       , testCase "withPid" testWithPid
       , testCase "exclusion" testExclusion
       , testCase "redirection" testRedirection
       ] mempty

ensureRemoved :: [FilePath] -> IO ()
ensureRemoved filepaths = forM_ filepaths $ \filepath -> do
    exists <- doesFileExist filepath
    when exists $ do
        removeFile filepath

-- Wait the given number of ms.
sleep :: Int -> IO ()
sleep n = threadDelay (n * 1000)

testFirst :: Assertion
testFirst = flip finally (ensureRemoved ["tmp"]) $ do
    let txtExp = "42"
    runDetached Nothing def $ do
        writeFile "tmp" txtExp
    sleep 500
    txt <- readFile "tmp"
    txt @?= txtExp

testWithPid :: Assertion
testWithPid = flip finally (ensureRemoved ["pid", "tmp"]) $ do
    let txtExp = "42"
    runDetached (Just "pid") def $ do
        writeFile "tmp" txtExp
    sleep 500
    txt <- readFile "tmp"
    txt @?= txtExp
    pid <- readFile "pid"
    null pid @?= False

testExclusion :: Assertion
testExclusion = flip finally (ensureRemoved ["pid", "tmp"]) $ do
    let txtExp = "ok"
    runDetached (Just "pid") def $ do
        handle (\(_ :: SomeException) -> writeFile "tmp" txtExp)
          (runDetached (Just "pid") def $ do
               writeFile "tmp" "failed")
    sleep 500
    txt <- readFile "tmp"
    txt @?= txtExp

testRedirection :: Assertion
testRedirection = flip finally (ensureRemoved ["tmp"]) $ do
    let txtExp = "ok"
    runDetached Nothing (ToFile "tmp") $ do
        putStr "ok"
    sleep 500
    txt <- readFile "tmp"
    txt @?= txtExp
