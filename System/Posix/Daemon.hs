-- | This module provides 'startDaemon' and 'stopDaemon' to facilitate
-- the creation of daemon programs.  Think @emacs --daemon@, or @adb@.
--
module System.Posix.Daemon (
        -- * Daemon control
        startDaemon
    ) where

import Prelude hiding ( FilePath )

import Control.Monad ( when )
import Data.Default ( Default(..) )
import Filesystem.Path.CurrentOS ( FilePath, encodeString )
import System.Directory ( doesFileExist )
import System.IO ( SeekMode(..) )
import System.Posix.IO ( openFd, OpenMode(..), defaultFileFlags, closeFd
                       , dupTo, stdInput, stdOutput, stdError, getLock
                       , LockRequest (..), createFile, setLock, fdWrite )
import System.Posix.Process ( getProcessID, forkProcess, createSession )

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

-- | Double-fork to create a well behaved daemon.  If @pidfile@ is
-- given, check/set pidfile; if we cannot obtain a lock on the file,
-- another process is already using it, so fail.  The @redirection@
-- parameter controls what to do with the standard channels (@stdin@,
-- @stderr@, and @stdout@).
--
-- See: <http://www.enderunix.org/docs/eng/daemon.php>
--
-- Note: All unnecessary fds should be close before calling this.
-- Otherwise, you get an fd leak.
startDaemon :: Maybe FilePath  -- ^ pidfile
            -> Redirection     -- ^ redirection
            -> IO ()           -- ^ program
            -> IO ()
startDaemon maybePidFile redirection program = do
    checkPidFile
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
            writePidFile
            -- run the daemon
            program
  where
    ignore act = act >> return ()

    remapFds = do
        let file = case redirection of
                     DevNull         -> "/dev/null"
                     ToFile filepath -> encodeString filepath
        fd <- openFd file ReadOnly Nothing defaultFileFlags
        mapM_ (dupTo fd) [stdInput, stdOutput, stdError]
        closeFd fd

    withPidFile act =
        case maybePidFile of
          Nothing      -> return ()
          Just pidFile -> act (encodeString pidFile)

    checkPidFile = withPidFile $ \pidFile -> do
        fe <- doesFileExist pidFile
        when fe $ do
            fd <- openFd pidFile WriteOnly Nothing defaultFileFlags
            ml <- getLock fd (ReadLock, AbsoluteSeek, 0, 0)
            closeFd fd
            case ml of
              Just (pid, _) -> fail (show pid ++ " already running")
              Nothing       -> return ()

    writePidFile = withPidFile $ \pidFile -> do
        fd <- createFile pidFile 777
        setLock fd (WriteLock, AbsoluteSeek, 0, 0)
        pid <- getProcessID
        ignore $ fdWrite fd (show pid)
