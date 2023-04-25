#!/bin/bash
ghcid --restart cli/Main.hs \
  --restart posterchild.cabal \
  --test ':t selectPostsByUser' \
  -c cabal repl exe:posterchild
