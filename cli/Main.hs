{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import Database.Posterchild
import Control.Monad (forM_)
import qualified Data.Map as Map

query :: SelectQuery
query =
  SelectQuery
    { selectFrom = SelectFromTable "posts"
    , selectFields = SelectFields
        [ SelectField (SelectColumn (ColumnRef "posts" "id")) Nothing
        , SelectField (SelectColumn (ColumnRef "posts" "title")) Nothing
        , SelectField (SelectColumn (ColumnRef "posts" "body")) (Just "body")
        , SelectField (SelectParam "foobar") Nothing
        , SelectField (SelectValue $ SqlInt 23) (Just "twenty_three")
        , SelectField (SelectSubquery $
            SelectQuery
              { selectFrom = SelectFromTable "users"
              , selectFields = SelectFields
                [ SelectField (SelectColumn (ColumnRef "users" "username")) (Just "username")
                ]
              , selectWhere =
                  WhereCompare
                    Equals
                    (SelectColumn (ColumnRef "users" "id"))
                    (SelectColumn (ColumnRef "posts" "user_id"))
              }
            ) (Just "username")
        ]
    , selectWhere = WhereTrue
    }

main :: IO ()
main = do
  let aliases = getQueryAliases query
  putStrLn "--- aliases ---"
  forM_ (Map.toList aliases) $ \(name, val) -> do
    print (name, val)
  putStrLn "--- constraints ---"
  forM_ (getQueryConstraints aliases query) print
  putStrLn "--- result type ---"
  print (getQueryResultType aliases query)
