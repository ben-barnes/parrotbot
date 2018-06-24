{-# LANGUAGE OverloadedStrings #-}

module Parrotbot.API (
  Config(..)
, Token(..)
, parrotApplication
) where

import Data.Aeson (
    (.:)
  , (.=)
  , FromJSON
  , ToJSON
  , eitherDecode
  , encode
  , object
  , parseJSON
  , toJSON
  , withObject
  )
import Data.Attoparsec.Text (
    Parser
  , char
  , string
  )
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Network.HTTP.Client (
    Manager
  , RequestBody(RequestBodyLBS)
  , httpLbs
  , method
  , parseRequest
  , requestBody
  , requestHeaders
  , responseBody
  )
import Network.HTTP.Types.Header (hAuthorization, hContentType)
import Network.HTTP.Types.Method (methodPost)
import Network.HTTP.Types.Status (
    badRequest400
  , forbidden403
  , methodNotAllowed405
  , ok200
  )
import Network.Wai (
    Application
  , Response
  , lazyRequestBody
  , requestMethod
  , responseLBS
  )
import Parrotbot.Language (
    parseAllParrot
  , parseAllSKI
  , renderParrot
  , renderSKI
  , repl
  )

import qualified Data.Attoparsec.Text as Atto (takeWhile)
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text (encodeUtf8)

data Config = Config {
  configManager :: Manager
, configAppToken :: Token
, configBotToken :: Token
}

data SlackEvent
  = SlackChallenge Challenge
  | SlackEvent EventWrapper
    deriving (Eq, Ord)

instance FromJSON SlackEvent where
  parseJSON v = withObject "SlackEvent" (\v' ->
    v' .: "type" >>= \tpe -> case tpe of
      "url_verification" -> SlackChallenge <$> parseJSON v
      "event_callback"   -> SlackEvent     <$> parseJSON v
      _ -> fail $ "Event type not recognised: " ++ (Text.unpack tpe)
    ) v

slackEventToken :: SlackEvent -> Token
slackEventToken (SlackChallenge c) = challengeToken c
slackEventToken (SlackEvent e)     = eventWrapperToken e

newtype Token = Token {
  getToken :: Text
} deriving (Eq, Ord)

instance FromJSON Token where
  parseJSON = fmap Token . parseJSON

instance ToJSON Token where
  toJSON = toJSON . getToken

data Challenge = Challenge {
  challengeToken :: Token
, challengeContent :: Text
} deriving (Eq, Ord)

instance FromJSON Challenge where
  parseJSON = withObject "Challenge" $ \v -> Challenge
    <$> v .: "token"
    <*> v .: "challenge"

newtype ChallengeResponse = ChallengeResponse {
  getChallengeResponse :: Text
} deriving (Eq, Ord)

instance ToJSON ChallengeResponse where
  toJSON r = object ["challenge" .= getChallengeResponse r]

data EventWrapper = EventWrapper {
  eventWrapperToken :: Token
, eventWrapperTeamId :: Text
, eventWrapperApiAppId :: Text
, eventWrapperEvent :: Event
, eventWrapperType :: Text
} deriving (Eq, Ord)

instance FromJSON EventWrapper where
  parseJSON = withObject "EventWrapper" $ \v -> EventWrapper
    <$> v .: "token"
    <*> v .: "team_id"
    <*> v .: "api_app_id"
    <*> v .: "event"
    <*> v .: "type"

data Event
  = AppMentionEvent AppMention
    deriving (Eq, Ord)

instance FromJSON Event where
  parseJSON v = withObject "Event" (\v' ->
    v' .: "type" >>= \tpe -> case tpe of
      "app_mention" -> AppMentionEvent <$> parseJSON v
      _ -> fail $ "Event type not recognised: " ++ (Text.unpack tpe)
    ) v

data AppMention = AppMention {
  appMentionUser :: Text
, appMentionText :: Text
, appMentionChannel :: Text
} deriving (Eq, Ord)

instance FromJSON AppMention where
  parseJSON = withObject "AppMention" $ \v -> AppMention
    <$> v .: "user"
    <*> v .: "text"
    <*> v .: "channel"

data AppMentionResponse = AppMentionResponse {
  appMentionResponseChannel :: Text
, appMentionResponseText :: Text
} deriving (Eq, Ord)

instance ToJSON AppMentionResponse where
  toJSON r = object [
      "channel" .= appMentionResponseChannel r
    , "text"    .= appMentionResponseText r
    ]

parrotApplication :: Config -> Application
parrotApplication config request respond =
  if requestMethod request == methodPost
    then do
      body <- lazyRequestBody request
      BLC.putStrLn $ "Got message: " <> body
      case eitherDecode body of
        Right e  -> parrotBot config e >>= respond
        Left err -> respond $ responseLBS badRequest400 [] $ BLC.pack err
    else respond $ responseLBS methodNotAllowed405 [] ""

parrotBot :: Config -> SlackEvent -> IO Response
parrotBot c e = withValidEvent c e $ handleSlackEvent c

withValidEvent
  :: (Applicative f)
  => Config
  -> SlackEvent
  -> (SlackEvent -> f Response)
  -> f Response
withValidEvent c e withEvent =
  if slackEventToken e == configAppToken c
    then withEvent e
    else pure $ responseLBS forbidden403 [] ""

handleSlackEvent :: Config -> SlackEvent -> IO Response
handleSlackEvent c (SlackChallenge sc) = return $ handleChallenge sc
handleSlackEvent c (SlackEvent e)     = handleEvent c e

handleChallenge :: Challenge -> Response
handleChallenge r =
  let content  = challengeContent r
      response = ChallengeResponse content
  in  responseLBS ok200 [(hContentType, "application/json; charset=utf-8")] (encode response)

handleEvent :: Config -> EventWrapper -> IO Response
handleEvent c e = case eventWrapperEvent e of
  AppMentionEvent a -> handleAppMention c a

handleAppMention :: Config -> AppMention -> IO Response
handleAppMention c a =
  let mgr = configManager c
      botToken = configBotToken c
      response = parrotMessage a
  in  do
    BLC.putStrLn $ "Responding with: " <> encode response
    baseReq <- parseRequest "https://slack.com/api/chat.postMessage"
    let req = baseReq {
        method = methodPost
      , requestBody = RequestBodyLBS (encode response)
      , requestHeaders = [
            (hContentType, "application/json; charset=utf-8")
          , (hAuthorization, "Bearer " <> (Text.encodeUtf8 . getToken $ botToken))
          ]
      }
    postMessageResponse <- httpLbs req mgr
    print $ responseBody postMessageResponse
    return $ responseLBS ok200 [] ""

parrotMessage :: AppMention -> AppMentionResponse
parrotMessage m =
  let content = appMentionText m
      channel = appMentionChannel m
  in  AppMentionResponse channel (parrot content)

parrot :: Text -> Text
parrot t =
  let t' = Text.drop 12 t
  in  case repl parseAllParrot renderParrot t' of
    Right result -> result
    Left _       -> case repl parseAllSKI renderSKI t of
      Right result -> result
      Left err     -> "Sorry, I couldn't understand that!"

parseMention :: Parser Text
parseMention = string "<@" *> Atto.takeWhile isAlphaNum <* char '>'
