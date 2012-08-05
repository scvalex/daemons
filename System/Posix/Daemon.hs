-- | This module provides a simple interface to creating, checking the
-- status of, and stopping background jobs.
--
-- Use 'runDetached' to start a background job.  For instance, here is
-- a daemon that peridically hits a webserver:
--
-- > import Control.Concurrent
-- > import Control.Monad
-- > import Data.Default
-- > import Data.Maybe
-- > import Network.BSD
-- > import Network.HTTP
-- > import Network.URI
-- > import System.Posix.Daemon
-- >
-- > main :: IO ()
-- > main = runDetached (Just "diydns.pid") def $ forever $ do
-- >     hostname <- getHostName
-- >     _ <- simpleHTTP
-- >              (Request { rqURI     = fromJust (parseURI "http://foo.com/dns")
-- >                       , rqMethod  = GET
-- >                       , rqHeaders = []
-- >                       , rqBody    = hostname })
-- >     threadDelay (600 * 1000 * 1000)
--
-- To check if the above job is running, use 'isRunning' with the same
-- pidfile:
--
-- > isRunning "diydns.pid"
--
-- Finally, to stop the above job (maybe because we're rolling a new
-- version of it), use 'kill':
--
-- > kill "diydns.pid"
--
-- As a side note, the code above is a script that the author uses as
-- a sort of homebrew dynamic DNS: the remote address is a CGI script
-- that records the IP addresses of all incoming requests in separate
-- files named after the contents of the requests; the addresses are
-- then viewable with any browser.
module System.Posix.Daemon (
        -- * Starting
        runDetached, Redirection(..),

        -- * Status
        isRunning,

        -- * Stopping
        kill, brutalKill
    ) where

import Prelude hiding ( FilePath )

import Control.Monad ( when )
import Data.Default ( Default(..) )
import System.Directory ( doesFileExist )
import System.FilePath ( FilePath )
import System.IO ( SeekMode(..), hFlush, stdout )
import System.Posix.Files ( stdFileMode )
import System.Posix.IO ( openFd, OpenMode(..), defaultFileFlags, closeFd
                       , dupTo, stdInput, stdOutput, stdError, getLock
                       , createFile
                       , LockRequest (..), setLock, fdWrite, fdRead )
import System.Posix.Process ( getProcessID, forkProcess, createSession )
import System.Posix.Signals ( Signal, signalProcess, sigQUIT, sigKILL )

-- | Where should the output (and input) of a daemon be redirected to?
-- (we can't just leave it to the current terminal, because it may be
-- closed, and that would kill the daemon).
--
-- When in doubt, just use 'def', the default value.
--
-- 'DevNull' causes the output to be redirected to @\/dev\/null@.  This
-- is safe and is what you want in most cases.
--
-- If you don't want to lose the output (maybe because you're using it
-- for logging), use 'ToFile', instead.
data Redirection = DevNull
                 | ToFile FilePath
                   deriving ( Show )

instance Default Redirection where
    def = DevNull

-- | Run the given action detached from the current terminal; this
-- creates an entirely new process.  This function returns
-- immediately.  Uses the double-fork technique to create a well
-- behaved daemon.  If @pidfile@ is given, check/write it; if we
-- cannot obtain a lock on the file, another process is already using
-- it, so fail.  The @redirection@ parameter controls what to do with
-- the standard channels (@stdin@, @stderr@, and @stdout@).
--
-- See: <http://www.enderunix.org/docs/eng/daemon.php>
--
-- Note: All unnecessary fds should be close before calling this.
-- Otherwise, you get an fd leak.
runDetached :: Maybe FilePath  -- ^ pidfile
            -> Redirection     -- ^ redirection
            -> IO ()           -- ^ program
            -> IO ()
runDetached maybePidFile redirection program = do
    -- check if the pidfile exists; fail if it does
    checkPidFile
    -- fork first child
    ignore $ forkProcess $ do
        -- create a new session and make this process its leader; see
        -- setsid(2)
        ignore $ createSession
        -- fork second child
        ignore $ forkProcess $ do
            -- create the pidfile
            writePidFile
            -- remap standard fds
            remapFds
            -- run the daemon
            program
  where
    ignore act = act >> return ()

    -- Remap the standard channels based on the @redirection@
    -- parameter.
    remapFds = do
        devnull <- openFd "/dev/null" ReadOnly Nothing defaultFileFlags
        ignore (dupTo devnull stdInput)
        closeFd devnull

        let file = case redirection of
                     DevNull         -> "/dev/null"
                     ToFile filepath -> filepath
        fd <- openFd file ReadWrite (Just stdFileMode) defaultFileFlags
        hFlush stdout
        mapM_ (dupTo fd) [stdOutput, stdError]
        closeFd fd

    -- Convert the 'FilePath' @pidfile@ to a regular 'String' and run
    -- the action with it.
    withPidFile act =
        case maybePidFile of
          Nothing      -> return ()
          Just pidFile -> act pidFile

    -- Check if the pidfile exists; fail if it does, and create it, otherwise
    checkPidFile = withPidFile $ \pidFile -> do
        running <- isRunning pidFile
        when running $ fail "already running"

    writePidFile = withPidFile $ \pidFile -> do
        fd <- createFile pidFile stdFileMode
        setLock fd (WriteLock, AbsoluteSeek, 0, 0)
        pid <- getProcessID
        ignore $ fdWrite fd (show pid)
        -- note that we do not close the fd; doing so would release
        -- the lock

-- | Return 'True' if the given file is locked by a process.  In our
-- case, returns 'True' when the daemon that created the file is still
-- alive.
isRunning :: FilePath -> IO Bool
isRunning pidFile = do
    dfe <- doesFileExist pidFile
    if dfe
      then do
          fd <- openFd pidFile ReadWrite Nothing defaultFileFlags
          -- is there an *incompatible* lock on the pidfile?
          ml <- getLock fd (WriteLock, AbsoluteSeek, 0, 0)
          (pid, _) <- fdRead fd 100
          closeFd fd
          case ml of
            Nothing -> do
                pid' <- getProcessID
                return (read pid == pid')
            Just _ -> do
                return True
      else do
          return False

-- | Send 'sigQUIT' to the process recorded in the pidfile.  This
-- gives the process a chance to close cleanly.
kill :: FilePath -> IO ()
kill = signalProcessByFilePath sigQUIT

-- | Send 'sigKILL' to the process recorded in the pidfile.  This
-- immediately kills the process.
brutalKill :: FilePath -> IO ()
brutalKill = signalProcessByFilePath sigKILL

-- | Send a signal to a process whose pid is recorded in a file.
signalProcessByFilePath :: Signal -> FilePath -> IO ()
signalProcessByFilePath signal pidFile = do
    pid <- readFile pidFile
    signalProcess signal (read pid)
