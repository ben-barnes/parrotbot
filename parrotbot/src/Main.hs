{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
) where

import Data.Char (isSpace)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp (run)
import Parrotbot.API (Config(Config), Token(Token), parrotApplication)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import qualified Data.Text as Text

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--app-token", appTokenFile, "--bot-token", botTokenFile] -> do
      appTokenString <- filter (not . isSpace) <$> readFile appTokenFile 
      botTokenString <- filter (not . isSpace) <$> readFile botTokenFile
      manager <- newManager tlsManagerSettings
      let appToken = Token . Text.pack $ appTokenString
          botToken = Token . Text.pack $ botTokenString
          config = Config manager appToken botToken
      run 8080 $ parrotApplication config
    _ -> hPutStrLn stderr "Usage: parrotbot --app-token file --bot-token file"
