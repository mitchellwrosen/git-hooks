{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative (pure)
import Control.Monad (void)
import Control.Monad.Trans.Either (eitherT)
import System.Directory (createDirectoryIfMissing, copyFile, setCurrentDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.Extras (CommandResult, fatalCall', systemCall', touchFile)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr, stdout)
import Test.HUnit

tempDir :: String
tempDir = ".test-environment"

testDir :: String
testDir = ".." </> "test"

gitHooksDir :: String
gitHooksDir = ".git-hooks"

main :: IO ()
main = runTestTT tests >>= \test_counts ->
    case (errors test_counts, failures test_counts) of
        (0, 0) -> exitSuccess
        _      -> exitFailure

tests :: Test
tests = TestList [ TestLabel "testFirstCommit"        testFirstCommit
                 , TestLabel "testNoMatchingPatterns" testNoMatchingPatterns
                 , TestLabel "testReverseExitCode"    testReverseExitCode
                 ]

-- setupTestEnvironment checkers_file
--
-- Enters an empty directory, creates a git repo, and commits |checkers_file| to
-- .git-hooks/checkers.json.
--
setupTestEnvironment :: String -> IO ()
setupTestEnvironment checkers_file =
    setupTestEnvironment' checkers_file >>
    void (fatalCall' "git" ["commit", "--no-verify", "-m", "1"])

-- setupTestEnvironment' checkers_file
--
-- Enters an empty directory, creates a git repo, git adds |checkers_file| after
-- copying it to .git-hooks/checkers.json, but doesn't commit it (so that the
-- first commit can be tested)
--
setupTestEnvironment' :: String -> IO ()
setupTestEnvironment' checkers_file =
    void (fatalCall' "rm" ["-rf", tempDir])                 >>
    createDirectoryIfMissing True (tempDir </> gitHooksDir) >>
    setCurrentDirectory tempDir                             >>
    void (fatalCall' "git" ["init"])                        >>
    copyFile "../pre-commit" ".git/hooks/pre-commit"        >>
    copyFile (testDir </> checkers_file) checkersDest       >>
    void (fatalCall' "git" ["add", checkersDest])
  where
    checkersDest :: String
    checkersDest = gitHooksDir </> "checkers.json"

-- teardownTestEnvironment
--
-- Cds up one level and removes the test environment directory.
--
teardownTestEnvironment :: IO ()
teardownTestEnvironment =
   setCurrentDirectory ".." >>
   void (fatalCall' "rm" ["-rf", tempDir])

isFirstCommit :: IO Bool
isFirstCommit =
    eitherT (const $ pure True) (const $ pure False) $
        systemCall' "git" ["rev-parse", "--verify", "HEAD"]

-- The first commit to a repo should fail.
testFirstCommit :: Test
testFirstCommit = TestCase $
    setupTestEnvironment' "test_first_commit.json" >>
    isFirstCommit >>=
        \case
            True ->
                -- setupTestEnvironment' left a file added but not committed.
                eitherT onFailure onSuccess (systemCall' "git" ["commit", "-am", "1"])
            False ->
                fatalCall' "git" ["rev-list", "HEAD", "--count"] >>= \count ->
                assertFailure (
                    "testFirstCommit running inside git repo with " ++ count ++
                    " commits already")
  where
    onFailure :: CommandResult -> IO ()
    onFailure _ = teardownTestEnvironment

    onSuccess :: CommandResult -> IO ()
    onSuccess _ = teardownTestEnvironment >>
                  assertFailure "First commit succeeded."

-- A checker that doesn't match any patterns should not run.
testNoMatchingPatterns :: Test
testNoMatchingPatterns = TestCase $
    setupTestEnvironment "test_no_matching_patterns.json" >>
    touchFile "not-a-txt-file" >>
    void (fatalCall' "git" ["add", "not-a-txt-file"]) >>
    eitherT onFailure onSuccess (systemCall' "git" ["commit", "-m", "foo"])
  where
    onFailure :: CommandResult -> IO ()
    onFailure (_, out, err) =
        teardownTestEnvironment >>
        hPutStrLn stdout out    >>
        hPutStrLn stderr err    >>
        assertFailure "Commit failed."

    onSuccess :: CommandResult -> IO ()
    onSuccess _ = teardownTestEnvironment

-- A checker with "reverse_exit_code: true" should run correctly.
testReverseExitCode :: Test
testReverseExitCode = TestCase $
    setupTestEnvironment "test_reverse_exit_code.json" >>
    writeFile "has-a-todo" "TODO" >>
    void (fatalCall' "git" ["add", "has-a-todo"]) >>
    eitherT onFailure onSuccess (systemCall' "git" ["commit", "-m", "foo"])
  where
    onFailure :: CommandResult -> IO ()
    onFailure _ = teardownTestEnvironment

    onSuccess :: CommandResult -> IO ()
    onSuccess _ = teardownTestEnvironment >>
                  assertFailure "Commit succeeded."

-- test no checkers.json
