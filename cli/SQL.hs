{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module SQL
where

import Database.Posterchild

selectPostsByUserQS :: String
selectPostsByUserQS = 
  "select posts.id as post_id " ++
  " , (select users.username from users where users.id = posts.user_id) as username " ++
  " , posts.title " ++
  " , posts.body " ++
  " from posts" ++
  " where posts.user_id = 1 " ++
  " and users.role = $1"

bloggSchema :: Schema
bloggSchema =
    Schema
      "blogg"
      [ ("users",
          Table
            { tableColumns =
                [ Column "id" SqlIntegerT NotNull
                , Column "username" SqlTextT NotNull
                , Column "role" SqlIntegerT NotNull
                ]
            , tableConstraints =
                [
                ]
            }
        )
      , ("posts",
          Table
            { tableColumns =
                [ Column "id" SqlIntegerT NotNull
                , Column "user_id" SqlIntegerT NotNull
                , Column "title" SqlTextT NotNull
                , Column "body" SqlTextT NotNull
                ]
            , tableConstraints =
                [
                ]
            }
        )
      ]
