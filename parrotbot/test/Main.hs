module Main (
  main
) where

import Control.Monad (unless)
import System.Exit (exitFailure)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout, stderr)

import qualified Test.Parrotbot.Language as Test.Parrotbot.Language

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  success <- Test.Parrotbot.Language.tests
  unless success exitFailure
