module System.Posix.Daemon (
        -- * Daemons
        Redirection(..), runDetached
    ) where

import Prelude hiding ( FilePath )

import Data.Default ( Default(..) )
import Filesystem.Path.CurrentOS ( FilePath, encodeString )
import System.IO ( SeekMode(..), hFlush, stdout )
import System.Posix.IO ( openFd, OpenMode(..), defaultFileFlags, closeFd
                       , dupTo, stdInput, stdOutput, stdError, getLock
                       , LockRequest (..), setLock, fdWrite )
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
    -- check if the pidfile exists; fail if it does, and create it,
    -- otherwise
    checkWritePidFile
    -- fork first child
    ignore $ forkProcess $ do
        -- create a new session and make this process its leader; see
        -- setsid(2)
        ignore $ createSession
        -- fork second child
        ignore $ forkProcess $ do
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
                     ToFile filepath -> encodeString filepath
        fd <- openFd file ReadWrite (Just 770) defaultFileFlags
        hFlush stdout
        mapM_ (dupTo fd) [stdOutput, stdError]
        closeFd fd

    -- Convert the 'FilePath' @pidfile@ to a regular 'String' and run
    -- the action with it.
    withPidFile act =
        case maybePidFile of
          Nothing      -> return ()
          Just pidFile -> act (encodeString pidFile)

    -- Check if the pidfile exists; fail if it does, and create it, otherwise
    checkWritePidFile = withPidFile $ \pidFile -> do
        fd <- openFd pidFile WriteOnly (Just 777) defaultFileFlags
        -- CR scvalex: We get the @ReadLock@, and set the
        -- @WriteLock@.  Is this correct?
        ml <- getLock fd (ReadLock, AbsoluteSeek, 0, 0)
        case ml of
          Just (pid, _) -> do
              closeFd fd
              fail (show pid ++ " already running")
          Nothing -> do
              setLock fd (WriteLock, AbsoluteSeek, 0, 0)
              pid <- getProcessID
              ignore $ fdWrite fd (show pid)
              closeFd fd
