{-# LANGUAGE OverloadedStrings #-}

module Parrotbot.API (
  Token(..)
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
import Data.Text (Text)
import Network.HTTP.Types.Header (hContentType)
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

import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as Text

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

parrotApplication :: Token -> Application
parrotApplication token request respond =
  if requestMethod request == methodPost
    then do
      body <- lazyRequestBody request
      case eitherDecode body of
        Right e  -> parrotBot token e >>= respond
        Left err -> respond $ responseLBS badRequest400 [] $ BLC.pack err
    else respond $ responseLBS methodNotAllowed405 [] ""

parrotBot :: Token -> SlackEvent -> IO Response
parrotBot t e = withValidEvent t e handleSlackEvent

withValidEvent
  :: (Applicative f)
  => Token
  -> SlackEvent
  -> (SlackEvent -> f Response)
  -> f Response
withValidEvent t e withEvent =
  if slackEventToken e == t
    then withEvent e
    else pure $ responseLBS forbidden403 [] ""

handleSlackEvent :: SlackEvent -> IO Response
handleSlackEvent (SlackChallenge c) = return $ handleChallenge c
handleSlackEvent (SlackEvent e)     = handleEvent e

handleChallenge :: Challenge -> Response
handleChallenge r =
  let content  = challengeContent r
      response = ChallengeResponse content
  in  responseLBS ok200 [(hContentType, "application/json")] (encode response)

handleEvent :: EventWrapper -> IO Response
handleEvent e = case eventWrapperEvent e of
  AppMentionEvent a -> handleAppMention a

handleAppMention :: AppMention -> IO Response
handleAppMention a = do
  putStrLn "Received app mention."
  return $ responseLBS ok200 [] ""
