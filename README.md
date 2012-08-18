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
    Daemon started on port 5000
    Just 23
    % addone 41
    Just 42

The two important functions above are `ensureDaemonRunning`, which
checks if a daemon named `addOne` is already running, and starts it if
not, and `runClient` which connects to the daemon running on
`localhost:5000`, passes it a number, and waits for the response.

What would I use this for?
--------------------------

 - You can use the `runDetached` from `System.Posix.Daemon` to turn
   your program into a daemon for Unix-like systems.  You'd want to do
   this for practically every program that's meant to run as a server.

 - You can use the functions from `Control.Pipe.C3`, `Socket`, and
   `Serialize` to communicate with running Haskell program.  At the
   simplest, you could query the program for its status, or instruct
   it to shutdown cleanly.  A more complex use would be adding a full
   REPL into a running Haskell process (think `erl -remsh`).

 - You can use the helpers from `System.Daemon` to trivially do the
   above.  Check out the following tutorials and examples for details.

Tutorials and examples
----------------------

 - [Memo](https://github.com/scvalex/daemons/blob/master/examples/Memo.md) -
   in which we write an in-memory key-value store,

 - [PMTQ](https://github.com/scvalex/daemons/blob/master/examples/PMTQ.hs)
   (Poor Man's Task Queue) - a task queue using the streaming interface
of `daemons`.

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
