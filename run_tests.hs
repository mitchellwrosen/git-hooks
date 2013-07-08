{-# LANGUAGE LambdaCase #-}

module Main where

import System.Extras (CommandResult, fatalCall_, systemCall')

import Control.Monad (void)
import Control.Monad.Trans.Either (eitherT)
import System.Directory (createDirectoryIfMissing, copyFile, setCurrentDirectory)
import System.FilePath ((</>))
import Test.HUnit

testDir :: String
testDir = ".test-environment"

setupTestEnvironment :: IO ()
setupTestEnvironment =
    fatalCall_ "rm" ["-rf", testDir] []                      >>
    createDirectoryIfMissing True (testDir </> ".git-hooks") >>
    setCurrentDirectory testDir                              >>
    fatalCall_ "git" ["init"] []                             >>
    copyFile "../pre-commit" ".git/hooks/pre-commit"

teardownTestEnvironment :: IO ()
teardownTestEnvironment =
   setCurrentDirectory ".." >>
   void (fatalCall_ "rm" ["-rf", testDir] [])

testFirstCommit :: Test
testFirstCommit = TestCase $
    setupTestEnvironment               >>
    fatalCall_ "touch" ["foo"] []      >>
    fatalCall_ "git" ["add", "foo"] [] >>

    eitherT onFailure onSuccess (systemCall' "git" ["commit", "-am", "\"foo\""])

  where
    onFailure :: CommandResult -> IO ()
    onFailure _ = teardownTestEnvironment

    onSuccess :: CommandResult -> IO ()
    onSuccess _ = teardownTestEnvironment >>
                  assertFailure "First commit erroneously succeeded."

tests :: Test
tests = TestList [ TestLabel "testFirstCommit" testFirstCommit ]

main :: IO ()
main = void $ runTestTT tests
