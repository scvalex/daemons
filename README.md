daemons
=======

> Daemons in Haskell made fun and easy

Example
-------

Here's
[AddOne](https://github.com/scvalex/daemons/blob/master/AddOne.hs), a
simple daemon that waits for a number and responds with the
incremented number.

    import Data.Default ( def )
    import System.Environment ( getArgs )
    import System.Daemon

    addOne :: Int -> IO Int
    addOne n = return (n + 1)

    main :: IO ()
    main = do
        ensureDaemonRunning "addOne" def addOne
        [n] <- getArgs
        res <- runClient "localhost" 5000 ((read n) :: Int)
        print (res :: Maybe Int)

Running it, we see:

    % addone 22
    Just 23
    % addone 41
    Just 42

The two important functions above are `startDaemon`, which checks if a
daemon named `addOne` is already running, and starts it if not, and
`runClient` which connects to the daemon running on `localhost:5000`,
passes it a number, and waits for the response.

For a less trivial example, see the in-memory key-value store,
[Memo](https://github.com/scvalex/daemons/blob/master/Memo.md).

For an example that uses the streaming interface of `daemons`, see
[PMTQ](https://github.com/scvalex/daemons/blob/master/PMTQ.hs) (Poor
Man's Task Queue).

Installation
------------

This package is on
[Hackage](http://hackage.haskell.org/package/daemons).  To install
it, run:

    cabal update
    cabal install daemons

Modules
-------

 - `Control.Pipe.C3` provides simple RPC-like wrappers for pipes.

 - `Control.Pipe.Serialize` provides pipes to serialize and
   deserialize streams of strict `ByteString`s using
   [cereal](http://hackage.haskell.org/package/cereal).

 - `Control.Pipe.Socket` provides functions to setup strict
   `ByteString` pipes around sockets.

 - `System.Daemon` provides a high-level interface to starting
   daemonized programs that are controlled through sockets.

 - `System.Posix.Daemon` provides a low-level interface to starting,
   and controlling detached jobs.

See also
--------

 - `pipes` [The Pipes Tutorial](http://hackage.haskell.org/packages/archive/pipes/latest/doc/html/Control-Pipe-Tutorial.html)

 - `C3` [Wikipedia](https://en.wikipedia.org/wiki/Command,_control,_and_communications#Command.2C_control_and_communications)
