module Main
where

import Test.Tasty

import qualified Database.Posterchild.Tests.TyCheck as TyCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "PosterChild"
          [ TyCheck.tests
          ]
