{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad (forM_, unless)
import Data.String.Utils (rstrip)
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>))
import System.IO (Handle, stderr)
import System.Process
   ( StdStream(..)
   , createProcess, readProcessWithExitCode, shell, std_out, system, waitForProcess
   )
import Text.Printf (hPrintf, printf)


-- exitFailure' command exit_code
--
-- Prints a failure message to stderr and then exits.
--
exitFailure' :: String -> Int -> IO a
exitFailure' command exit_code = hPrintf stderr msg >> exitFailure
  where
    msg = unlines [ "######################"
                  , "PRE-COMMIT HOOK FAILED"
                  , "#######################"
                  , printf "'%s' failed with exit code %d" command exit_code
                  ]

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
         ExitFailure exit_code -> stashApply >> exitFailure' command exit_code
         ExitSuccess           -> return ()

-- readProcess' command args input
--
-- Runs |command| with |args| and |input|, returning the standard output, or
-- exiting on failure.
--
readProcess' :: String -> [String] -> String -> IO String
readProcess' command args input= do
    readProcessWithExitCode command args input >>=
        \case
            (ExitFailure exit_code, out, err) -> exitFailure' command exit_code
            (ExitSuccess, out, _)             -> return out

-- toplevel
--
-- Gets the root directory of the current git repository.
--
topLevel :: IO String
topLevel = rstrip <$> readProcess' "git" ["rev-parse", "--show-toplevel"] []

stashSave :: IO ()
stashSave = system' "git stash --keep-index --include-untracked --quiet"

stashApply :: IO ()
stashApply = system' "git reset --hard --quiet" >>
             system' "git stash pop --index --quiet"

-- Checker command output
data Checker = Checker String String
data PresubmitCheck = Check     Checker
                    | LangCheck Checker String

checkers :: [Checker]
checkers = [ Checker "./run_tests.sh" "Running run_tests.sh..." ]

main :: IO ()
main =
    stashSave >>

    forM_ checkers (
        \(Checker command output) ->
            putStrLn output >>
            check command
    ) >>

    stashApply
