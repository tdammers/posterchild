{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Posterchild.Tests.TyCheck
where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Database.Posterchild.TyCheck
import Database.Posterchild.Syntax
import Database.Posterchild.Parser.Select

tests :: TestTree
tests = testGroup "TyCheck"
          [ testGroup "Select"
            [ testCase "Simple" testSelectSimple
            , testCase "UnqualifiedColumnNameSingleTable" testSelectUnqualifiedColumnNameSingleTable
            , testCase "UnqualifiedColumnNameAliased" testSelectUnqualifiedColumnNameAliased
            , testCase "UnusedTable" testSelectUnusedTable
            ]
          ]

testQueryType :: String -> Either TypeError SelectQueryTy -> Assertion
testQueryType sql expected = do
  sq <- parseSelect sql sql
  let actual = runTC $ tcSelectQuery sq
  assertEqual "Query type" expected actual

testSelectSimple :: Assertion
testSelectSimple =
  testQueryType
    "SELECT tbl.col FROM tbl"
    $ Right SelectQueryTy
        { selectQueryParamsTy =
            []
        , selectQueryResultTy =
            [ ("tbl.col", ColumnRefTy "tbl" "col")
            ]
        , selectQueryConstraintsTy =
            [ TableExists "tbl"
            , ColumnExists (ColumnRef "tbl" "col")
            ]
        }

testSelectUnqualifiedColumnNameSingleTable :: Assertion
testSelectUnqualifiedColumnNameSingleTable =
  testQueryType
    "SELECT col FROM tbl"
    $ Right SelectQueryTy
        { selectQueryParamsTy =
            []
        , selectQueryResultTy =
            [ ("col", ColumnRefTy "tbl" "col")
            ]
        , selectQueryConstraintsTy =
            [ TableExists "tbl"
            , ColumnExists (ColumnRef "tbl" "col")
            ]
        }

testSelectUnqualifiedColumnNameAliased :: Assertion
testSelectUnqualifiedColumnNameAliased =
  testQueryType
    "SELECT tbl.a AS col FROM tbl INNER JOIN blah"
    $ Right SelectQueryTy
        { selectQueryParamsTy =
            []
        , selectQueryResultTy =
            [ ("col", ColumnRefTy "tbl" "a")
            ]
        , selectQueryConstraintsTy =
            [ TableExists "blah"
            , TableExists "tbl"
            , ColumnExists (ColumnRef "tbl" "a")
            ]
        }

testSelectUnusedTable :: Assertion
testSelectUnusedTable = do
  testQueryType
    "SELECT 1 AS a FROM tbl"
    $ Right SelectQueryTy
        { selectQueryParamsTy =
            []
        , selectQueryResultTy =
            [ ("a", MonoTy SqlSmallIntT)
            ]
        , selectQueryConstraintsTy =
            [ TableExists "tbl"
            ]
        }
