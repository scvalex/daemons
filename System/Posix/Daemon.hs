module System.Posix.Daemon (
        -- * Daemons
        Redirection(..), runDetached, isRunning, kill, brutalKill
    ) where

import Prelude hiding ( FilePath )

import Control.Monad ( when )
import Data.Default ( Default(..) )
import Data.Maybe ( isJust )
import System.Directory ( doesFileExist )
import System.FilePath ( FilePath )
import System.IO ( SeekMode(..), hFlush, stdout )
import System.Posix.Files ( stdFileMode )
import System.Posix.IO ( openFd, OpenMode(..), defaultFileFlags, closeFd
                       , dupTo, stdInput, stdOutput, stdError, getLock
                       , createFile
                       , LockRequest (..), setLock, fdWrite )
import System.Posix.Process ( getProcessID, forkProcess, createSession )
import System.Posix.Signals ( Signal, signalProcess, sigQUIT, sigKILL )

-- FIXME Add usage example

-- | Where should the output (and input) of a daemon be redirected to?
-- (we can't just leave it to the current terminal, because it may be
-- closed, and that would kill the daemon).
--
-- When in doubt, just use @def@, the default value.
--
-- @DevNull@ causes the output to be redirected to @/dev/null@.  This
-- is safe and is what you want in most cases.
--
-- If you don't want to lose the output (maybe because you're using it
-- for logging), use @ToFile@, instead.
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

-- FIXME There's some weird behaviour when the process that has locked
-- the file (or its ancestors, or its descendents) use 'isRunning'.
--
-- The semantics of 'fnctl' are "try to aquire the requested lock; if
-- there is an incompatible lock in place, return it".  Of course,
-- this means that the process that acquired the lock sees it as
-- unlocked.

-- | Return 'True' if the given file is locked by a process.  In our
-- case, returns 'True' when the daemon that created the file is still
-- alive.
isRunning :: FilePath -> IO Bool
isRunning pidFile = do
    dfe <- doesFileExist pidFile
    if dfe
      then do
          fd <- openFd pidFile WriteOnly Nothing defaultFileFlags
          ml <- getLock fd (WriteLock, AbsoluteSeek, 0, 0)
          closeFd fd
          return (isJust ml)
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

signalProcessByFilePath :: Signal -> FilePath -> IO ()
signalProcessByFilePath signal pidFile = do
    pid <- readFile pidFile
    signalProcess signal (read pid)
