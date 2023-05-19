#!/bin/bash
while (true)
do
    ghcid --restart cli/Main.hs \
      --restart posterchild.cabal \
      --restart cabal.project \
      --test ':! ghcid --test ":t selectSightingsByUser (Proxy :: Proxy Birdtracker)" -c cabal repl exe:posterchild' \
      -c cabal repl lib:posterchild
    sleep 0.5
    inotifywait -e modify posterchild.cabal
done
