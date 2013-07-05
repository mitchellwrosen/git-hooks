{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import Debug.Trace

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero, when)
import Data.Aeson (FromJSON, Value(..), parseJSON, (.:), (.:?), eitherDecode')
import Data.Maybe (fromJust, isJust)
import Data.String.Utils (join)
import System.Exit (ExitCode(..), exitFailure, exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode, system)
import Text.Printf (hPrintf, printf)

import qualified Data.ByteString.Lazy as B

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

-- execute on_fail command
--
-- Runs "command" as a shell command. In the case of ExitFailure, print
-- |on_fail| (if not Nothing), apply the saved stash, and exit.
--
-- The main difference between this function and system' is this function is
-- used for commands that are run after saving a stash (it calls stashApply).
-- There may be a clean way to abstract out the common functionality.
--
execute :: Maybe String -> String -> IO ()
execute on_fail command = do
   system command >>=
      \case
         ExitSuccess           -> return ()
         ExitFailure exit_code ->
            when (isJust on_fail) (hPutStrLn stderr $ fromJust on_fail) >>
            stashApply >>
            exitFailure' exit_code

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

-- A Checker represents a program that runs with either no arguments or one
-- argument (a filename), returning a non-zero exit code on failure.
--
-- No-argument checkers have no patterns to match (chPatterns is Nothing). They
-- are for repository-scope sanity checks such as
--
--      - Attempting to commit to master
--      - Existence of non-ascii filenames
--      - Offending whitespace
--      - Non-compiling code
--      - Failing test(s)
--
-- Single-argument checkers have a list of patterns to match (chPatterns is Just
-- patterns), and are run on each matching file being committed. They are for
-- file-scope sanity checks such as
--
--      - Linters
--      - Existence of TODO/FIXME
--      - Logging, debugging, printfs, console.log(), etc.
--
data Checker = Checker
    { chCommand  :: String
    , chOutput   :: String
    , chPatterns :: Maybe [String]
    , chOnFail   :: Maybe String
    }

instance FromJSON Checker where
    parseJSON (Object o) = Checker          <$>
                           o .:  "command"  <*>
                           o .:  "output"   <*>
                           o .:? "patterns" <*>
                           o .:? "on_fail"
    parseJSON _ = mzero

-- check checker
--
-- Runs checker, inspecting |patterns| to determine if it's a repo-scope or
-- file-scope checker.
--
check :: Checker -> IO ()
check (Checker command output maybe_patterns maybe_on_fail) =
    putStrLn output >>
    case maybe_patterns of
        -- File-scope sanity checks.
        Just patterns ->
            readProcess'
                "git"                                                 -- command
                (words "diff --staged --name-only --diff-filter=ACM") -- arguments
                [] >>=                                                -- input
            readProcess'
                "grep"                                                -- command
                [join "\\|" patterns] >>=                             -- arguments
            mapM_ (execute maybe_on_fail . printf "%s %s" command) . lines
        -- Repository-scope sanity checks.
        Nothing -> execute maybe_on_fail command

main :: IO ()
main =
    readCheckers >>= \checkers ->

    checkFirstCommit                    >>
    checkEmptyCommit                    >>
    stashSave                           >>
    mapM_ check checkers                >>
    putStrLn "Presubmit checks passed." >>
    stashApply

  where
    readCheckers :: IO [Checker]
    readCheckers =
        B.readFile ".git-hooks/checkers.json" >>= \contents ->
        case eitherDecode' contents of
            Right checkers -> return checkers
            Left  err      -> hPutStrLn stderr err >> exitFailure
