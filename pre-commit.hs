{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad (unless)
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


-- exitFailure' msgs exit_code
--
-- Prints a failure message (salted with |msgs|, which represent one message
-- split into lines) to stderr and then exits.
--
exitFailure' :: [String] -> IO a
exitFailure' msgs = hPrintf stderr msg' >> exitFailure
  where
    msg' = unlines $ [ "##########################"
                     , "# PRE-COMMIT HOOK FAILED #"
                     , "##########################"
                     ] ++ msgs

-- system' command
--
-- Runs "command" as a shell command, exiting on ExitFailure.
--
system' :: String -> IO ()
system' command = do
   system command >>=
      \case
         ExitFailure exit_code -> exitFailure' $
            [ printf "'%s' failed with exit code %d" command exit_code ]
         ExitSuccess -> return ()

-- readProcess' command args input
--
-- Runs |command| with |args| and |input|, returning the standard output, or
-- exiting on failure.
--
readProcess' :: String -> [String] -> String -> IO String
readProcess' command args input= do
    readProcessWithExitCode command args input >>=
        \case
            (ExitFailure exit_code, out, err) -> exitFailure' $
                [ printf "stdout: %s" out
                , printf "stderr: %s" err
                , printf "'%s' failed with exit code %d" command exit_code
                ]
            (ExitSuccess, out, _) -> return out

-- toplevel
--
-- Gets the root directory of the current git repository.
--
topLevel :: IO String
topLevel = rstrip <$> readProcess' "git" ["rev-parse", "--show-toplevel"] []

-- getRunTestsPath
--
-- Gets the path to run_tests.sh in the root directory of the current git
-- repository. Exits if run_tests.sh doesn't exist.
--
getRunTestsPath :: IO String
getRunTestsPath = do
   run_tests_path <- fmap (</> "run_tests.sh") topLevel
   run_tests_exists <- doesFileExist run_tests_path

   unless run_tests_exists $
      exitFailure' $ [ printf "'%s' not found." run_tests_path
                     , "Please create it and hook your test suite into it."
                     , "A failing test must cause run_tests.sh to return non-zero exit code."
                     ]

   return run_tests_path

main :: IO ()
main = getRunTestsPath       >>=
       return . ("bash " ++) >>=
       system'               >>
       return ()
