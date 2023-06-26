#!/bin/bash
while (true)
do
    ghcid --restart cli/Main.hs \
      --restart posterchild.cabal \
      --restart cabal.project \
      --restart 'test-src/*.hs' \
      --test ':! ghcid --test "Main.main" -c cabal repl posterchild-tests' \
      -c cabal repl lib:posterchild
    sleep 0.5
    inotifywait -e modify posterchild.cabal
done
