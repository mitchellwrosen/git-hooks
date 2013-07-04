{-# LANGUAGE LambdaCase #-}

module Main where

import Debug.Trace

import Control.Monad (forM_)
import System.Exit (ExitCode(..), exitWith, exitSuccess)
import System.IO (stderr)
import System.Process (readProcessWithExitCode, system)
import Text.Printf (hPrintf, printf)

traceShow' :: Show a => a -> a
traceShow' a = traceShow a a

hookFailedBanner :: [String]
hookFailedBanner = [ "####################################################"
                   , "#                                                  #"
                   , "# PRE-COMMIT HOOK FAILED                           #"
                   , "#                                                  #"
                   , "# There is a way to bypass this hook, but I won't  #"
                   , "# tell you what it is, because you should only use #"
                   , "# it if you know what you are doing.               #"
                   , "#                                                  #"
                   , "####################################################"
                   ]

-- exitFailure' exit_code
--
-- Prints a failure banner to stderr and then exits.
--
exitFailure' :: Int -> IO a
exitFailure' exit_code =
    hPrintf stderr (unlines hookFailedBanner) >>
    exitWith (ExitFailure exit_code)

-- system' command
--
-- Runs "command" as a shell command, exiting on ExitFailure.
--
system' :: String -> IO ()
system' command = do
   system command >>=
      \case
         ExitSuccess           -> return ()
         ExitFailure exit_code -> exitFailure' exit_code

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
         ExitFailure exit_code -> stashApply >> exitFailure' exit_code

-- readProcess' command args input
--
-- Runs |command| with |args| and |input|, returning the standard output, or
-- exiting on failure.
--
readProcess' :: String -> [String] -> String -> IO String
readProcess' command args input = do
    readProcessWithExitCode command args input >>=
        \case
            (ExitSuccess,           out, _) -> return out
            (ExitFailure exit_code, _,   _) ->
                putStrLn command' >>
                exitFailure' exit_code
          where
            command' :: String
            command' = concat [ command, unwords args, " << ", input ]

-- checkFirstCommit
--
-- Checks to see if this is the first commit to the repository, and fail with a
-- helpful error message if so.
--
checkFirstCommit :: IO ()
checkFirstCommit =
    system "git rev-parse --verify HEAD >/dev/null 2>&1" >>=
        \case
            ExitSuccess -> return ()
            ExitFailure exit_code ->
                (putStrLn . unlines)
                    [ " *"
                    , " * Many pre-commit checks rely on a valid HEAD,"
                    , " * so the Easy Way Out is to disable this hook"
                    , " * for your first commit."
                    , " *"
                    , " * Know that this commit will go unchecked, and"
                    , " * with great power comes great responsibility..."
                    , " *"
                    , " *     git commit --no-verify"
                    , " *"
                    ] >>
                exitFailure' exit_code

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
stashSave = system' "git stash --keep-index --include-untracked --quiet"

stashApply :: IO ()
stashApply = -- system' "git reset --quiet --hard" >>
             system' "git stash pop --index --quiet"

-- Checker command output
data Checker = Checker String String

checkers :: [Checker]
checkers = [ Checker ".git-hooks/check_on_master.sh"        "Checking current branch..."
           , Checker ".git-hooks/check_ascii_filenames.sh"  "Checking for non-ascii filenames..."
           , Checker "git diff-index --cached --check HEAD" "Checking for bad whitespace..."
           , Checker "redo pre-commit"                      "Building..."
           , Checker "./run_tests.sh"                       "Running run_tests.sh..."
           ]

-- LangChecker command pattern output
data LangChecker = LangChecker String String String

langCheckers :: [LangChecker]
langCheckers = [ ] -- LangChecker "hlint" "\\.hs$" "Running hlint..." ]

main :: IO ()
main =
    checkFirstCommit >>
    checkEmptyCommit >>
    stashSave        >>

    forM_ checkers (
        \(Checker command output) ->
            putStrLn output >>
            check command
    ) >>

    forM_ langCheckers (
        \(LangChecker command pattern output) ->
            putStrLn output >>
            readProcess' "git" ["diff", "--staged", "--name-only", "--diff-filter=ACM"] [] >>=
            readProcess' "grep" [pattern] >>=
            mapM_ (check . printf "%s %s" command) . lines
    ) >>

    putStrLn "Presubmit checks passed." >>

    stashApply
