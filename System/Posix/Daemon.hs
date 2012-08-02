-- | This module provides 'startDaemon' and 'stopDaemon' to facilitate
-- the creation of daemon programs.  Think @emacs --daemon@, or @adb@.
--
module System.Posix.Daemon (
        -- * Daemon control
        startDaemon
    ) where

import Control.Monad ( when )
import System.Directory ( doesFileExist )
import System.IO ( SeekMode(..) )
import System.Posix.IO ( openFd, OpenMode(..), defaultFileFlags, closeFd
                       , dupTo, stdInput, stdOutput, stdError, getLock
                       , LockRequest (..), createFile, setLock, fdWrite )
import System.Posix.Process ( getProcessID, forkProcess, createSession )

-- | Double-fork to create a well behaved daemon.  If @pidfile@ is
-- given, check/set pidfile; if we cannot obtain a lock on the file,
-- another process is already using it, so fail.
--
-- See: <http://www.enderunix.org/docs/eng/daemon.php>
--
-- Note: All unnecessary fds should be close before calling this.
-- Otherwise, you get an fd leak.
startDaemon :: FilePath  -- ^ pidfile
            -> IO ()     -- ^ program
            -> IO ()
startDaemon pidFile program = do
    checkRunning
    -- fork first child
    ignore $ forkProcess $ do
        -- create a new session and make this process its leader; see
        -- setsid(2)
        ignore $ createSession
        -- fork second child
        ignore $ forkProcess $ do
            -- remap standard fds
            remapFds
            -- lock file
            setRunning
            -- run the daemon
            program
  where
    ignore act = act >> return ()

    remapFds = do
        devnull <- openFd "/dev/null" ReadOnly Nothing defaultFileFlags
        mapM_ (dupTo devnull) [stdInput, stdOutput, stdError]
        closeFd devnull

    checkRunning = do
        fe <- doesFileExist pidFile
        when fe $ do
            fd <- openFd pidFile WriteOnly Nothing defaultFileFlags
            ml <- getLock fd (ReadLock, AbsoluteSeek, 0, 0)
            closeFd fd
            case ml of
              Just (pid, _) -> fail (show pid ++ " already running")
              Nothing       -> return ()

    setRunning = do
        fd <- createFile pidFile 777
        setLock fd (WriteLock, AbsoluteSeek, 0, 0)
        pid <- getProcessID
        ignore $ fdWrite fd (show pid)
