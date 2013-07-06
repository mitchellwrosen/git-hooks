{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import Debug.Trace

import System.Extras

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error
import Data.Aeson (FromJSON, Value(..), parseJSON, (.:), (.:?), eitherDecode')
import System.Exit (ExitCode(..), exitFailure, exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr, stdout)
import System.Process (system)
import Text.Printf (printf)

import qualified Data.ByteString.Lazy as B
import qualified Data.String.Utils    as S

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
                (hPutStrLn stderr . unlines)
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
                exitWith (ExitFailure exit_code)

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
stashSave =
    runErrorT (system' "git stash --keep-index --include-untracked --quiet") >>=
        \case
            Right _   -> return ()
            Left  err -> hPutStrLn stderr err >> exitFailure

stashApply :: IO ()
stashApply = -- system' "git reset --quiet --hard" >>
    runErrorT (system' "git stash pop --index --quiet") >>=
        \case
            Right _   -> return ()
            Left  err -> hPutStrLn stderr err >> exitFailure

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
check :: Checker -> ErrorT String IO ()
check (Checker command output maybe_patterns _) =
    liftIO (putStrLn output) >>
    case maybe_patterns of
        -- File-scope sanity checks.
        Just patterns ->
            readProcess' "git diff --staged --name-only --diff-filter=ACM" [] >>=
            readProcess' ("grep" ++ S.join "\\|" patterns) >>=
            mapM_ (system' . printf "%s %s" command) . lines

        -- Repository-scope sanity checks.
        Nothing -> system' command

main :: IO ()
main =
    readCheckers >>= \checkers ->

    checkFirstCommit >>
    checkEmptyCommit >>

    stashSave >>
    runErrorT (mapM_ check checkers) >>= \result ->
        stashApply >>
        case result of
            Right () -> hPutStrLn stdout "Presubmit checks passed."           >> exitSuccess
            Left err -> hPutStrLn stderr ("Presubmit checks failed: " ++ err) >> exitFailure

  where
    readCheckers :: IO [Checker]
    readCheckers =
        B.readFile ".git-hooks/checkers.json" >>= \contents ->
        case eitherDecode' contents of
            Right checkers -> return checkers
            Left  err      -> hPutStrLn stderr err >> exitFailure
