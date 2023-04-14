#!/bin/sh
ghcid --restart cli/Main.hs \
  --restart posterchild.cabal \
  --test ':! cabal run' \
  -c cabal repl lib:posterchild
