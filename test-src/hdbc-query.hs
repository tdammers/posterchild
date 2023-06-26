{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTSyntax #-}
module Main
where

import Database.Posterchild as P
import Database.Posterchild.SchemaConstraints
import Database.Posterchild.Parser
import Database.Posterchild.TH
import Database.Posterchild.Driver.Class
import Database.Posterchild.Driver.HDBC
import Data.HList
import Data.Proxy

import Database.HDBC
import Database.HDBC.PostgreSQL

$(mkSchema $
  Schema "example"
    [ ( "bar"
      , Table
          { tableColumns =
              [ Column "foo" P.SqlIntegerT NotNull ]
          , tableConstraints =
              []
          }
      )
    ]
  )

$(mkSelectQueryDec "selectExample" "SELECT foo FROM bar WHERE 1")

main =
  withPostgreSQL "bogus-dsn" $ \conn -> do
    selectExample (Proxy @Example) (HDBCDriver conn) HNil

