## HBefunge-93 ##
First project in haskell, dummy Befunge-93 interpreter.

## Compilation ##

```
#!shell
cabal sandbox init
cabal install -j
cp .cabal-sandbox/bin/HBefunge .
 # or
ln -s .cabal-sandbox/bin/HBefunge HBefunge
```

## How does it work ##
```
#!shell
./HBefunge tests/helloworld.bef
```

## Dependencies ##
* cabal
* System.Random
* Data.Vector
