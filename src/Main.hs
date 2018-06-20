{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
) where

import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Parrotbot.API (Token(Token), parrotApplication)
import Parrotbot.Language (
    parseAllParrot
  , parseAllSKI
  , renderParrot
  , renderSKI
  , repl
  )
import System.Environment (getArgs)
import System.IO (hPutStr, stderr)
import Web.Slack (
    Event(Message)
  , SlackBot
  , SlackConfig(SlackConfig)
  , _slackApiToken
  , runBot
  )
import Web.Slack.Message (sendMessage)

import qualified Data.Text as Text

main :: IO ()
main = run 8080 (parrotApplication $ Token "")

-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     [token] -> runBot (config token) parrotBot ()
--     _       -> hPutStr stderr "Usage: parrotbot slack-api-token"

config :: String -> SlackConfig
config token = SlackConfig { _slackApiToken =  token }

parrotBot :: SlackBot ()
parrotBot (Message cid _ msg _ _ _) =
  case Text.stripPrefix "parrot: " msg of
    Nothing    -> return ()
    Just input -> case parrotRepl input of
      Right result -> sendMessage cid result
      Left _       -> sendMessage cid "Sorry, I couldn't understand that!"
parrotBot _ = return ()

parrotRepl :: Text -> Either Text Text
parrotRepl t =
  case repl parseAllParrot renderParrot t of
    Right result -> Right result
    Left _       -> repl parseAllSKI renderSKI t

