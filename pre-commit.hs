{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (forM_)
import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import System.IO (stderr)
import System.Process (readProcessWithExitCode, system)
import Text.Printf (hPrintf, printf)

hookFailedBanner :: [String]
hookFailedBanner = [ "######################"
                   , "PRE-COMMIT HOOK FAILED"
                   , "######################"
                   ]

-- exitFailure' command exit_code
--
-- Prints a failure message to stderr and then exits.
--
exitFailure' :: String -> Int -> IO a
exitFailure' command exit_code = hPrintf stderr msg >> exitFailure
  where
    msg = unlines $ hookFailedBanner ++
                    [ printf "'%s' failed with exit code %d" command exit_code ]

-- system' command
--
-- Runs "command" as a shell command, exiting on ExitFailure.
--
system' :: String -> IO ()
system' command = do
   system command >>=
      \case
         ExitFailure exit_code -> exitFailure' command exit_code
         ExitSuccess           -> return ()

-- check command
--
-- Runs "command" as a shell command, applying the saved stash and exiting on
-- ExitFailure.
--
check :: String -> IO ()
check command = do
   system command >>=
      \case
         ExitSuccess           -> return ()
         ExitFailure exit_code -> stashApply >> exitFailure' command exit_code

-- readProcess' command args input
--
-- Runs |command| with |args| and |input|, returning the standard output, or
-- exiting on failure.
--
readProcess' :: String -> [String] -> String -> IO String
readProcess' command args input= do
    readProcessWithExitCode command args input >>=
        \case
            (ExitSuccess,           out, _) -> return out
            (ExitFailure exit_code, _,   _) -> exitFailure' command exit_code

-- checkFirstCommit
--
-- Checks to see if this is the first commit to the repository, and fail with a
-- helpful error message if so.
checkFirstCommit :: IO ()
checkFirstCommit =
    system "git rev-parse --verify HEAD >/dev/null 2>&1" >>=
        \case
            ExitSuccess -> return ()
            ExitFailure _ -> putStrLn $
                unlines $ hookFailedBanner ++
                          [ "Many pre-commit checks rely on a valid HEAD,"
                          , "so the Easy Way Out is to disable this hook"
                          , "for your first commit."
                          , ""
                          , "Know that this commit will go unchecked, and"
                          , "with great power comes great responsibility..."
                          , ""
                          , "(use git commit --no-verify)"
                          ]

-- checkEmptyCommit
--
-- Checks whether or not this is an empty commit. If it is, there is no need
-- to do any pre-commit check, so exit with success.
--
checkEmptyCommit :: IO ()
checkEmptyCommit =
    system "git diff --quiet --staged" >>=
        \case
            ExitSuccess   -> exitSuccess
            ExitFailure _ -> return ()

stashSave :: IO ()
stashSave = system' "git stash --quiet --keep-index --include-untracked"

stashApply :: IO ()
stashApply = -- system' "git reset --quiet --hard" >>
             system' "git stash pop --quiet --index"

-- Checker command output
data Checker = Checker String String

checkers :: [Checker]
checkers = [ Checker ".git-hooks/check_ascii_filenames.sh" "Checking for non-ascii filenames..."
           , Checker ".git-hooks/check_whitespace.sh"      "Checking for bad whitespace..."
           , Checker "./run_tests.sh"                      "Running run_tests.sh..."
           , Checker "redo pre-commit"                     "Compiling..."
           ]

-- LangChecker command pattern output
data LangChecker = LangChecker String String String

langCheckers :: [LangChecker]
langCheckers = [ LangChecker "hlint" "*.hs" "Running hlint..." ]

main :: IO ()
main =
    checkFirstCommit >>
    checkEmptyCommit >>
    stashSave >>

    forM_ checkers (
        \(Checker command output) ->
            putStrLn output >>
            check command
    ) >>

    putStrLn "Presubmit checks passed." >>

    stashApply
