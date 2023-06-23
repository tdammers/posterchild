module Main
where

import Test.Tasty

import qualified Database.Posterchild.Tests.TyCheck as TyCheck
import qualified Database.Posterchild.Tests.TH as TH

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "PosterChild"
          [ TyCheck.tests
          , TH.tests
          ]
