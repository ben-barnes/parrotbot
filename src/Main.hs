{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, char, choice, endOfInput, many1, parseOnly, string)
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.Semigroup ((<>))
import Data.Text (Text)
import System.Environment (getArgs)
import System.IO (hPutStr, stderr)
import Web.Slack (Event(Message), SlackBot, SlackConfig(SlackConfig), _slackApiToken, runBot)
import Web.Slack.Message (sendMessage)

import qualified Data.Text as Text

main :: IO ()
main = do
  args <- getArgs
  case args of
    [token] -> runBot (config token) parrotBot ()
    _       -> hPutStr stderr "Usage: parrotbot slack-api-token"

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
  case repl parseInputParrot renderParrot t of
    Right result -> Right result
    Left _       -> repl parseInputSKI renderSKI t

data Term
  = S
  | K
  | I
  | A Term Term

isApplication :: Term -> Bool
isApplication (A _ _) = True
isApplication _       = False

eval :: Term -> Term
eval S = S
eval K = K
eval I = I
eval (A t1 t2) = case (eval t1, eval t2) of
  (I, t')                  -> eval t'
  ((A K t1'), _)           -> eval t1'
  ((A (A S t1') t2'), t3') ->
    let t1'' = eval t1'
        t2'' = eval t2'
        t3'' = eval t3'
        t4'' = eval (A t1'' t3'')
        t5'' = eval (A t2'' t3'')
    in  eval (A t4'' t5'')
  (t1', t2')               -> A t1' t2'

renderSKI :: Term -> Text
renderSKI S = "S"
renderSKI K = "K"
renderSKI I = "I"
renderSKI (A t1 t2) =
  if isApplication t2
    then renderSKI t1 <> "(" <> renderSKI t2 <> ")"
    else renderSKI t1 <> renderSKI t2

renderParrot :: Term -> Text
renderParrot S = ":aussie_parrot:"
renderParrot K = ":fast_parrot:"
renderParrot I = ":parrot:"
renderParrot (A t1 t2) =
  if isApplication t2
    then renderParrot t1 <> "(" <> renderParrot t2 <> ")"
    else renderParrot t1 <> renderParrot t2

parseSKI :: Parser Term
parseSKI =
  let parseS = S <$ char 'S'
      parseK = K <$ char 'K'
      parseI = I <$ char 'I'
      parseP = char '(' *> parseSKI <* char ')'
      parseT = choice [parseP, parseS, parseK, parseI]
  in  foldl1 A <$> many1 parseT

parseInputSKI :: Text -> Either Text Term
parseInputSKI = first Text.pack . parseOnly (parseSKI <* endOfInput)

parseParrot :: Parser Term
parseParrot =
  let parseS = S <$ (string ":aussie_parrot:" <|> string ":s:")
      parseK = K <$ (string ":fast_parrot:"   <|> string ":k:")
      parseI = I <$ (string ":parrot:"        <|> string ":i:")
      parseP = char '(' *> parseParrot <* char ')'
      parseT = choice [parseP, parseS, parseK, parseI]
  in  foldl1 A <$> many1 parseT

parseInputParrot :: Text -> Either Text Term
parseInputParrot = first Text.pack . parseOnly (parseParrot <* endOfInput)

repl :: (Text -> Either Text Term) -> (Term -> Text) -> Text -> Either Text Text
repl parse render = fmap (render . eval) . parse . Text.filter (not . isSpace)
