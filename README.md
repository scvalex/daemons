daemons
=======

> Daemons in Haskell made fun and easy

Example
-------

See [Memo](https://github.com/scvalex/daemons/blob/master/Memo.hs)
for an example.

Modules
-------

 - `Control.Pipe.Serialize` provides pipes to serialize and
   deserialize streams of strict `ByteString`s using
   [cereal](http://hackage.haskell.org/package/cereal).

 - `Control.Pipe.Socket` provides functions to setup strict
   `ByteString` pipes around sockets.

See also
--------

 - `pipes` [The Pipes Tutorial](http://hackage.haskell.org/packages/archive/pipes/latest/doc/html/Control-Pipe-Tutorial.html)

 - `C3` [Wikipedia](https://en.wikipedia.org/wiki/Command,_control,_and_communications#Command.2C_control_and_communications)
