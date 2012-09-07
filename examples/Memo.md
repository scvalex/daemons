Memo
====

> A simple in-memory key-value store

Welcome to the first `daemons` tutorial, in which we walk through
writing an in-memory key-value store with an RPC-like interface.  The
code for this tutorial is
[Memo.hs](https://github.com/scvalex/daemons/blob/master/examples/Memo.hs).

Concretely, we want a program such that:

 - `memo put x 42` associates the value `42` with the key `x`, and

 - `memo get x` returns the value `42`.

First of all, the extensions and imports:

    {-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
    

We need `DeriveGenerics` for
[cereal](http://hackage.haskell.org/package/cereal) to generate
serializers and deserializers automatically, and we enable
`OverloadedStrings` because it makes working with `ByteString`s much
nicer.

    module Main where
    
    import Control.Concurrent.MVar ( MVar, newMVar, modifyMVar )
    import Data.ByteString.Char8 ( ByteString )
    import Data.Default ( def )
    import Data.Serialize ( Serialize )
    import Data.String ( fromString )
    import qualified Data.Map as M
    import GHC.Generics
    import System.Environment ( getArgs )
    import System.Daemon
    

Our key-value store will be a `Map ByteString ByteString` and we'll
store it in an `MVar` to synchronize concurrent accesses.  Instead of
handcrafting a binary protocol for our daemon, we take the easy road
and generate it automatically with `Data.Serialize` and
[GHC.Generics](http://www.haskell.org/ghc/docs/7.4.2/html/users_guide/generic-programming.html).

We import `System.Daemon` which is the high-level interface to the
`daemons` library.  The daemons' configuration is an instance of
[Data.Default](http://hackage.haskell.org/package/data-default), so
we'll be able to use the defaults.

    data Command = Put ByteString ByteString
                 | Get ByteString
                   deriving ( Generic, Show )
    
    instance Serialize Command
    

We define a datatype for the `put <key> <value>` and `get <key>`
commands.  We let GHC derive the `Generics` instance, which gives us a
pure Haskell representation of the type; this is used by the
`Serialize` instance to generate all the necessary binary
serialization and deserialization code.

    data Response = Failed String
                  | Value ByteString
                    deriving ( Generic, Show )
    
    instance Serialize Response
    

Similarly, we define a datatype for the possible responses.  These can
either be values requested by `get <key>`, or failure messages.

    type Book = M.Map ByteString ByteString
    
    handleCommand :: MVar Book -> Command -> IO Response
    handleCommand bookVar comm = modifyMVar bookVar $ \book -> return $

Our "book" is just a map of `ByteString`s; our command handler takes
this map and a command, and returns a response.

Whenever the daemon receives a command, it spawns a new thread and
runs the command handler.  We want to share the book between these
concurrent calls to the handler, so we stick it in an
[MVar](http://www.haskell.org/ghc/docs/7.4.2/html/libraries/base/Control-Concurrent-MVar.html).

An `MVar` is basically a thread-safe box which holds at most *one*
item.  We use `modifyMVar` which takes the book out of the `MVar`,
runs our function with it, and puts the returned book back in the
`MVar`.

        case comm of
          Get key -> ( book
                     , maybe (Failed "not found") Value (M.lookup key book) )

A `get <key>` command does not change the book, so we just return it.
We look up the key and return its value or a failure message.

          Put key value -> ( M.insert key value book
                           , Value "ok" )
    

A `put <key> <value>` command inserts the key-value pair into the
book, and returns a confirmation message.

    main :: IO ()
    main = do
        bookVar <- newMVar M.empty
        let options = def { daemonPort = 7856 }
        ensureDaemonRunning "memo" options (handleCommand bookVar)

Before doing anything else, we need to ensure that the daemon is
running: we create an empty book, customize the daemon's default
options, and finally start it.  Note that `ensureDaemonRunning` checks
if the daemon is running and starts it otherwise; so, the daemon will
be started the first time the program is run, and all later runs will
use the initial daemon.

        args <- getArgs
        let args' = map fromString args

Now it's time to handle the user input.  First, we convert all the
arguments to `ByteString`s for ease of use.

        res <- case args' of
          ["get", key]        -> runClient "localhost"  7856 (Get key)
          ["put", key, value] -> runClient "localhost"  7856 (Put key value)
          _                   -> error "invalid command"

Next, we parse the arguments into a command and send it to the daemon.
We call `runClient` with the port we gave earlier to
`ensureDaemonRunning` and with the parsed command.

        print (res :: Maybe Response)
    

Finally, we print the returned response.  Note that `runClient` is
polymorphic in its return so we *need* to specify the type of the
response.

Now let's see it in action:

    {-
    % dist/build/memo/memo get apples
    Daemon started on port 7856
    Just (Failed "not found")
    
    % dist/build/memo/memo put apples 23
    Just (Value "ok")
    
    % dist/build/memo/memo get apples
    Just (Value "23")
    -}

To recap, we:

 - wrote data-types for commands and responses and gave them
  `Serialize` instances,

 - wrote a handler that takes a command and returns a response,

 - ensured that our daemon is running with `ensureDaemonRunning`, and

 - sent commands and received responses with `runClient`.

This tutorial illustrates the basic concepts behind `daemons`, but
hides a powerful feature: the interface is *streaming*.  See the
[Queue](https://github.com/scvalex/daemons/blob/master/examples/Queue.hs)
(Poor Man's Task Queue) tutorial for an example use of the streaming
interface.
