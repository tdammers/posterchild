{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.Monad (forM_)
import Text.Printf
import Language.Haskell.TH (runQ, pprint)
import Database.HDBC.PostgreSQL (Connection)
import Data.Proxy

import Database.Posterchild
import Database.Posterchild.SchemaConstraints
import Database.Posterchild.Parser
import Database.Posterchild.TH
import Database.Posterchild.Driver.Class
import Data.HList

import SQL

$(mkSchema birdtrackerSchema)

queryStr :: String
queryStr = selectSightingsByUserQS

$(mkSelectQueryDec "selectSightingsByUser" selectSightingsByUserQS)

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

  putStrLn . pprint =<< runQ (mkSelectQueryDec "selectSightingsByUser" queryStr)
