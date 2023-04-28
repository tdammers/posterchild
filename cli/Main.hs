{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Control.Monad (forM_)
import Text.Printf
import Language.Haskell.TH (runQ, pprint)

import Database.Posterchild
import Database.Posterchild.SchemaConstraints
import Database.Posterchild.Parser
import Database.Posterchild.TH

queryStr :: String
queryStr = 
  "select posts.id as post_id " ++
  " , (select users.username from users where users.id = posts.user_id) as username " ++
  " , posts.title " ++
  " , posts.body " ++
  " from posts" ++
  " where posts.user_id = 1 " ++
  " and users.role = $1"

$(mkSelectQuery "selectPostsByUser" $
    "select posts.id as post_id " ++
    " , (select users.username from users where users.id = posts.user_id) as username " ++
    " , posts.title " ++
    " , posts.body " ++
    " from posts" ++
    " where posts.user_id = 1 " ++
    " and users.role = $1 " ++
    " and posts.id = $2 "
  )

main :: IO ()
main = do
  putStrLn "--- input ---"
  putStrLn queryStr
  query <- parseSelect "<<QUERY>>" queryStr
  putStrLn "--- AST ---"
  print query
  queryT <- either (error . show) return $ runTC $ tcSelectQuery query
  putStrLn "--- params ---"
  forM_ (selectQueryParamsTy queryT) $ \(ParamName name, ty) ->
    printf "$%s : %s\n" name (show ty)
  putStrLn "--- result type ---"
  forM_ (selectQueryResultTy queryT) $ \(ColumnName name, ty) ->
    printf "%s : %s\n" name (show ty)
  putStrLn "--- constraints ---"
  mapM_ print $ selectQueryConstraintsTy queryT

  putStrLn . pprint =<< runQ (mkSelectQuery "selectPostsByUser" queryStr)
