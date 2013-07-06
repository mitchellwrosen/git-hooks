{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module System.Extras where

import Control.Monad.Error
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stdout)
import System.Process (readProcessWithExitCode)

-- system' command on_fail
--
-- Runs |command| as a shell command, returning Left if ExitFailure.
--
system' :: String -> ErrorT String IO ()
system' command = readProcess' command [] >> return ()

-- readProcess' command input
--
-- Runs |command| with |input|, returning the standard output. In the case
-- of failure, prints the output (because some programs are dumb and print to
-- stdout upon failing) and returns Left stderr.
--
readProcess' :: String -> String -> ErrorT String IO String
readProcess' command input =
    liftIO (readProcessWithExitCode command' (words args) input) >>=
        \case
            (ExitSuccess,   out, _)   -> return out
            (ExitFailure _, out, err) -> liftIO (hPutStrLn stdout out) >>
                                         throwError err
  where
    (command', args) = span (/= ' ') command
