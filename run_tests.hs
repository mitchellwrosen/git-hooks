{-# LANGUAGE LambdaCase #-}

module Main where

import System.Extras (CommandResult, fatalCall', systemCall')

import Control.Applicative (pure)
import Control.Monad (void)
import Control.Monad.Trans.Either (eitherT)
import System.Directory (createDirectoryIfMissing, copyFile, setCurrentDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr, stdout)
import Test.HUnit

testDir :: String
testDir = ".test-environment"

gitHooksDir :: String
gitHooksDir = ".git-hooks"

-- setupTestEnvironment checkers_str
--
-- Enters an empty directory, creates a git repo, and commits |checkers_str| to
-- .git-hooks/checkers.json.
--
setupTestEnvironment :: String -> IO ()
setupTestEnvironment checkers_str =
    setupTestEnvironment' checkers_str >>
    void (fatalCall' "git" ["commit", "--no-verify", "-m", "1"])

-- setupTestEnvironment' checkers_str
--
-- Enters an empty directory, creates a git repo, git adds |checkers_str| after
-- writing it to .git-hooks/checkers.json, but doesn't commit it (so that the
-- first commit can be tested)
--
setupTestEnvironment' :: String -> IO ()
setupTestEnvironment' checkers_str =
    void (fatalCall' "rm" ["-rf", testDir])                 >>
    createDirectoryIfMissing True (testDir </> gitHooksDir) >>
    setCurrentDirectory testDir                             >>
    void (fatalCall' "git" ["init"])                        >>
    copyFile "../pre-commit" ".git/hooks/pre-commit"        >>
    writeFile checkersDest checkers_str                     >>
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
   void (fatalCall' "rm" ["-rf", testDir])

--------------------------------------------------------------------------------

isFirstCommit :: IO Bool
isFirstCommit =
    eitherT (const $ pure True) (const $ pure False) $
        systemCall' "git" ["rev-parse", "--verify", "HEAD"]

-- The first commit to a repo should fail.
testFirstCommit :: Test
testFirstCommit = TestCase $
    setupTestEnvironment' "[]" >>
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

--------------------------------------------------------------------------------

-- A checker that doesn't match any patterns should not run.
testNoMatchingPatterns :: Test
testNoMatchingPatterns = TestCase $
    setupTestEnvironment checkers_str >>
    writeFile "not-a-txt-file" "" >>
    void (fatalCall' "git" ["add", "not-a-txt-file"]) >>
    eitherT onFailure onSuccess (systemCall' "git" ["commit", "-m", "foo"])
  where
    checkers_str :: String
    checkers_str =
        unlines [ "["
                , "   {"
                , "      \"command\"  : \"!\","
                , "      \"args\"     : [\"echo\"],"
                , "      \"output\"   : \"\","
                , "      \"patterns\" : [\"\\\\.txt$\"]"
                , "   }"
                , "]"
                ]

    onFailure :: CommandResult -> IO ()
    onFailure (_, out, err) =
        teardownTestEnvironment >>
        hPutStrLn stdout out    >>
        hPutStrLn stderr err    >>
        assertFailure "Commit failed."

    onSuccess :: CommandResult -> IO ()
    onSuccess _ = teardownTestEnvironment

-- test no checkers.json

tests :: Test
tests = TestList [ TestLabel "testFirstCommit"        testFirstCommit
                 , TestLabel "testNoMatchingPatterns" testNoMatchingPatterns
                 ]

main :: IO ()
main = runTestTT tests >>= \test_counts ->
    case (errors test_counts, failures test_counts) of
        (0, 0) -> exitSuccess
        _      -> exitFailure
