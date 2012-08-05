{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Default
import Data.Monoid
import System.Directory
import System.Posix.Daemon
import System.Posix.Process

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

main :: IO ()
main = defaultMainWithOpts
       [ testCase "firstRun" testFirst
       , testCase "withPid" testWithPid
       -- , testCase "isRunning" testIsRunning
       , testCase "exclusion" testExclusion
       , testCase "release" testRelease
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
        pid <- getProcessID
        pid' <- readFile "pid"
        if show pid == pid'
          then writeFile "tmp" txtExp
          else writeFile "tmp" "wrong pid recorded"
    sleep 500
    txt <- readFile "tmp"
    txt @?= txtExp
    pid <- readFile "pid"
    null pid @?= False

-- testIsRunning :: Assertion
-- testIsRunning = flip finally (ensureRemoved ["pid", "tmp"]) $ do
--     runDetached (Just "pid") def $ do
--         running <- isRunning "pid"
--         writeFile "tmp" (show running)
--         sleep 10000
--     sleep 500
--     running <- isRunning "pid"
--     running @?= True
--     txt <- readFile "tmp"
--     txt @?= "True"

testExclusion :: Assertion
testExclusion = flip finally (ensureRemoved ["pid", "tmp"]) $ do
    let txtExp = "ok"
    runDetached (Just "pid") def $ do
        sleep 1000
    sleep 500
    handle (\(_ :: SomeException) -> writeFile "tmp" txtExp)
        (runDetached (Just "pid") def $ do
             writeFile "tmp" "failed")
    sleep 500
    txt <- readFile "tmp"
    txt @?= txtExp

testRelease :: Assertion
testRelease = flip finally (ensureRemoved ["pid", "tmp"]) $ do
    let txtExp = "ok"
    runDetached (Just "pid") def $ do
        writeFile "tmp" txtExp
    sleep 500
    txt <- readFile "tmp"
    txt @?= txtExp
    let txtExp' = "ok"
    runDetached (Just "pid") def $ do
        writeFile "tmp" txtExp'
    sleep 500
    txt' <- readFile "tmp"
    txt' @?= txtExp'

testRedirection :: Assertion
testRedirection = flip finally (ensureRemoved ["tmp"]) $ do
    let txtExp = "ok"
    runDetached Nothing (ToFile "tmp") $ do
        putStr "ok"
    sleep 500
    txt <- readFile "tmp"
    txt @?= txtExp
