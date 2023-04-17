{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Vector as Vector
import Control.Monad (forM_)
import qualified Data.Map as Map
import Text.Printf

import Database.Posterchild
import Database.Posterchild.Parser

main :: IO ()
main = do
  query <- parseSelect
            "<<QUERY>>" $
            "select posts.id as post_id " ++
            " , (select users.username from users where users.id = posts.user_id) as username " ++
            " , posts.title " ++
            " , posts.body " ++
            " from posts" ++
            " where posts.user_id = 1"
  let aliases = getQueryAliases query
  putStrLn "--- aliases ---"
  forM_ (Map.toList aliases) $ \(ColumnName name, val) -> do
    printf "%s -> %s\n" name (show val)
  putStrLn "--- constraints ---"
  case getQueryConstraints aliases query of
    Left err ->
      putStrLn $ "TYPE ERROR: " ++ err
    Right constraints ->
      Vector.mapM_ print constraints
  putStrLn "--- result type ---"
  case getQueryResultType aliases query of
    Left err ->
      putStrLn $ "TYPE ERROR: " ++ err
    Right rty ->
      Vector.mapM_ print rty
