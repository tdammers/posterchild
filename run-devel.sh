#!/bin/bash
while (true)
do
    ghcid --restart cli/Main.hs \
      --restart posterchild.cabal \
      --restart cabal.project \
      --test ':! ghcid --test ":t selectPostsByUser (Proxy :: Proxy Blogg)" -c cabal repl exe:posterchild' \
      -c cabal repl lib:posterchild
    sleep 0.5
    inotifywait -e modify posterchild.cabal
done
