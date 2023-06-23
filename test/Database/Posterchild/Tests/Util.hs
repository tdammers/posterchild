module Database.Posterchild.Tests.Util
where

import Test.Tasty.HUnit
import System.Process
import System.Exit
import Control.Monad (when)

assertCompiles :: FilePath -> Assertion
assertCompiles sourceFile = do
  (exitCode, stdout, stderr) <- runGHC [sourceFile]
  assertEqual (stdout ++ "\n" ++ stderr) ExitSuccess exitCode

assertDoesNotCompile :: FilePath -> Assertion
assertDoesNotCompile sourceFile = do
  (exitCode, _, _) <- runGHC [sourceFile]
  assertBool "exit code" (exitCode /= ExitSuccess)

runGHC :: [String] -> IO (ExitCode, String, String)
runGHC args =
  readProcessWithExitCode "cabal" ("exec" : "--" : "ghc" : args) ""
