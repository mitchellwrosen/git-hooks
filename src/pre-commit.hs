{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import Debug.Trace

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (forM_, mzero, unless, void)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Either (eitherT)
import Data.Aeson (FromJSON, Value(..), parseJSON, (.:), (.:?), eitherDecode')
import System.Exit (exitFailure, exitSuccess, exitWith)
import System.Extras
    ( CommandResult, System, fatalCall', systemCall', systemCallReverse'
    , systemCallWithDefault
    )
import System.IO (hPutStrLn, stderr, stdout)

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

main :: IO ()
main =
    readCheckers         >>= \checkers ->
    doPreStashSaveChecks >>
    stashSave            >>
    eitherT onFailure onSuccess (mapM_ check checkers)
  where
    readCheckers :: IO [Checker]
    readCheckers =
        B.readFile ".git-hooks/checkers.json" >>= \contents ->
        case eitherDecode' contents of
            Right checkers -> pure checkers
            Left  err      -> hPutStrLn stderr err >> exitFailure

    onFailure :: CommandResult -> IO ()
    onFailure (_, out, err) =
        stashApply >>
        hPutStrLn stderr (unlines $ out : err : hookFailedBanner) >>
        exitFailure

    onSuccess :: () -> IO ()
    onSuccess () = stashApply >> hPutStrLn stderr "Presubmit checks passed."

doPreStashSaveChecks :: IO ()
doPreStashSaveChecks =
    checkIsFirstCommit >>
    checkIsEmptyCommit

checkIsFirstCommit :: IO ()
checkIsFirstCommit =
    eitherT onFailure onSuccess $
        systemCall' "git" ["rev-parse", "--verify", "HEAD"]
  where
    onFailure :: CommandResult -> IO ()
    onFailure (exit_code, _, _) =
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
        exitWith exit_code

    onSuccess :: CommandResult -> IO ()
    onSuccess _ = pure ()

checkIsEmptyCommit :: IO ()
checkIsEmptyCommit =
    eitherT (const $ pure ()) (const exitSuccess) $
        systemCall' "git" ["diff", "--staged", "--quiet"]

stashSave :: IO ()
stashSave = void $ fatalCall' "git" (words "stash --keep-index --include-untracked --quiet")

stashApply :: IO ()
stashApply = -- fatalCall "git" (words "reset --hard --quiet") [] >>
             void $ fatalCall' "git" (words "stash pop --index --quiet")

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
    { chCommand         :: String
    , chArgs            :: [String]
    , chOutput          :: String
    , chPatterns        :: Maybe [String]
    , chReverseExitCode :: Maybe Bool
    , chOnFail          :: Maybe String
    }

instance FromJSON Checker where
    parseJSON (Object o) = Checker                   <$>
                           o .:  "command"           <*>
                           o .:  "args"              <*>
                           o .:  "output"            <*>
                           o .:? "patterns"          <*>
                           o .:? "reverse_exit_code" <*>
                           o .:? "on_fail"
    parseJSON _ = mzero

-- check checker
--
-- Runs checker, inspecting |patterns| to determine if it's a repo-scope or
-- file-scope checker.
--
check :: Checker -> System ()
-- File-scope sanity checks.
check (Checker command args output (Just patterns) maybe_reverse_exit_code _) =
    case maybe_reverse_exit_code of
        Just True -> check' systemCallReverse'
        _         -> check' systemCall'
  where
    check' :: (String -> [String] -> System CommandResult) -> System ()
    check' sysCall =
        liftIO (fatalCall' "git"
                           [ "diff"
                           , "--staged"
                           , "--name-only"
                           , "--diff-filter=ACM"
                           ]) >>= \out ->
        lines <$>
            lift (systemCallWithDefault
                    "" -- Replace failed greps with "no matches".
                    "grep"
                    [S.join "\\|" patterns]
                    out
                 ) >>= \filenames ->

        -- Don't print a checker's output unless at least one file matches.
        unless (null filenames) (liftIO $ hPutStrLn stdout output) >>

        forM_ filenames (\filename ->
            liftIO (hPutStrLn stdout ("   " ++ filename)) >>
            sysCall command (args ++ [filename])
        )
-- Repository-scope sanity checks.
check (Checker command args output Nothing maybe_reverse_exit_code _) =
    case maybe_reverse_exit_code of
        Just True -> check' systemCallReverse'
        _         -> check' systemCall'
  where
    check' :: (String -> [String] -> System CommandResult) -> System ()
    check' sysCall = liftIO (hPutStrLn stdout output) >>
                     void (sysCall command args)

