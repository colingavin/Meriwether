Meriwether - A Haskell General Game Player
==========================================

This repository includes the beginnings of a general game player. So far, we have
a parser for GDL, and most of GDL query done.

Install / Development
---------------------
Use the cabal build system. 

You will need to install the `logict` and `hunit` packages.

To run the unit tests do:
```
cabal configure --enable-tests
cabal build
cabal test
```

Todo
----

- State machine representation of games
- Playing logic (probably UCT with learned heuristics)
- Web protocol implementation
