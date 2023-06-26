module Database.Posterchild.Tests.TH
where

import Test.Tasty
import Test.Tasty.HUnit

import Database.Posterchild.Tests.Util

tests :: TestTree
tests = testGroup "TH"
          [ testCase "simple-select" $ assertCompiles "test-src/simple-select.hs"
          , testCase "hdbc-query" $ assertCompiles "test-src/hdbc-query.hs"
          ]
