{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Database.Posterchild.Tests.TyCheck
where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Database.Posterchild.TyCheck
import Database.Posterchild.Syntax
import Database.Posterchild.Parser

import qualified Data.Text as Text

tests :: TestTree
tests = testGroup "TyCheck"
          [ testGroup "Select"
            [ testCase "Simple" testSelectSimple
            , testCase "UnqualifiedColumnNameSingleTable" testSelectUnqualifiedColumnNameSingleTable
            , testCase "UnqualifiedColumnNameAliased" testSelectUnqualifiedColumnNameAliased
            , testCase "UnusedTable" testSelectUnusedTable
            , testCase "WhereColumnExists" testSelectWhereColumnExists
            , testCase "Join" testSelectJoin
            , testCase "GroupBy" testSelectGroupBy
            , testCase "SubquerySimple" testSelectSubquerySimple
            ]
          ]

testQueryType :: String -> Either TypeError SelectQueryTy -> Assertion
testQueryType sql expected = do
  sq <- either (error . show) return $ parseSelect sql (Text.pack sql)
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

testSelectGroupBy :: Assertion
testSelectGroupBy =
  testQueryType
    "SELECT tbl.col FROM tbl GROUP BY tbl.col"
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
    "SELECT tbl.a AS col FROM tbl INNER JOIN blah ON 1"
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

testSelectWhereColumnExists :: Assertion
testSelectWhereColumnExists = do
  testQueryType
    "SELECT 1 AS a FROM tbl WHERE col = 1"
    $ Right SelectQueryTy
        { selectQueryParamsTy =
            []
        , selectQueryResultTy =
            [ ("a", MonoTy SqlSmallIntT)
            ]
        , selectQueryConstraintsTy =
            [ TableExists "tbl"
            , ColumnExists (ColumnRef "tbl" "col")
            , EqTypes (ColumnRefTy "tbl" "col") (MonoTy SqlSmallIntT)
            ]
        }

testSelectJoin :: Assertion
testSelectJoin = do
  testQueryType
    "SELECT a.foo FROM a INNER JOIN b ON b.a_id = a.id WHERE b.bar = 1"
    $ Right SelectQueryTy
        { selectQueryParamsTy =
            []
        , selectQueryResultTy =
            [ ("a.foo", ColumnRefTy "a" "foo")
            ]
        , selectQueryConstraintsTy =
            [ TableExists "a"
            , TableExists "b"
            , ColumnExists (ColumnRef "a" "foo")
            , ColumnExists (ColumnRef "a" "id")
            , ColumnExists (ColumnRef "b" "a_id")
            , ColumnExists (ColumnRef "b" "bar")
            , EqTypes (ColumnRefTy "a" "id") (ColumnRefTy "b" "a_id")
            , EqTypes (ColumnRefTy "b" "bar") (MonoTy SqlSmallIntT)
            ]
        }

testSelectSubquerySimple :: Assertion
testSelectSubquerySimple = do
  testQueryType
    "SELECT a.foo FROM (SELECT foo FROM b WHERE b.id = 1) AS a"
    $ Right SelectQueryTy
        { selectQueryParamsTy =
            []
        , selectQueryResultTy =
            [ ("a.foo", ColumnRefTy "a" "foo")
            ]
        , selectQueryConstraintsTy =
            [ TableExists "b"
            , ColumnExists (ColumnRef "b" "foo")
            , ColumnExists (ColumnRef "b" "id")
            , EqTypes (ColumnRefTy "b" "id") (MonoTy SqlSmallIntT)
            ]
        }
