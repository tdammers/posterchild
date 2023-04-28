#!/bin/bash
while (true)
do
    ghcid --restart cli/Main.hs \
      --restart posterchild.cabal \
      --test ':! ghcid --test ":t selectPostsByUser" -c cabal repl exe:posterchild' \
      -c cabal repl lib:posterchild
    sleep 0.5
    inotifywait -e modify posterchild.cabal
done
