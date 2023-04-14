{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import Database.Posterchild
import Control.Monad (forM_)

query :: SelectQuery
query =
  SelectQuery
    { selectFrom = SelectFromTable "posts"
    , selectFields = SelectFields
        [ SelectField (SelectColumn "posts" "id") Nothing
        , SelectField (SelectColumn "posts" "title") Nothing
        , SelectField (SelectColumn "posts" "body") Nothing
        ]
    }

main :: IO ()
main = do
  forM_ (getQueryConstraints query) print
