#!/bin/bash
while (true)
do
    ghcid --restart cli/Main.hs \
      --restart posterchild.cabal \
      --restart cabal.project \
      --test ':! ghcid --test "Spec.main" -c cabal repl posterchild-tests' \
      -c cabal repl lib:posterchild
    sleep 0.5
    inotifywait -e modify posterchild.cabal
done
