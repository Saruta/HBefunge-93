# README #

## HBefunge-93 ##
First project in haskell, dummy Befunge-93 interpreter.

## Compilation ##

```
#!shell
cabal sandbox init
cabal install -j
cp .cabal-sandbox/bin/HBefunge .
```

## How does it work ##
```
#!shell
./HBefunge tests/helloworld.bef
```

## Dependencies ##
* cabal
* Data.Vector 