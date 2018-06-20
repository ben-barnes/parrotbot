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

data EventWrapper
  = EventWrapperChallenge ChallengeRequest
  | EventWrapperEvent Event
    deriving (Eq, Ord)

data Event
  = EventAppMention AppMention
    deriving (Eq, Ord)

instance FromJSON Event where
  parseJSON v = withObject "Event" (\v' ->
    v' .: "type" >>= \tpe -> case tpe of
      "app_mention" -> EventAppMention <$> (v' .: "event" >>= parseJSON)
      _             -> fail $ "Invalid event type: " ++ (Text.unpack tpe)
    ) v
      
data AppMention = AppMention {
  appMentionUser :: User
, appMentionText :: MessageText
, appMentionChannel :: Channel
} deriving (Eq, Ord)

instance FromJSON AppMention where
  parseJSON = withObject "AppMention" $ \v -> AppMention
    <$> v .: "user"
    <*> v .: "text"
    <*> v .: "channel"

newtype User = User {
  getUser :: Text
} deriving (Eq, Ord)

instance FromJSON User where
  parseJSON = fmap User . parseJSON

newtype MessageText = MessageText {
  getMessageText :: Text
} deriving (Eq, Ord)

instance FromJSON MessageText where
  parseJSON = fmap MessageText . parseJSON

newtype Channel = Channel {
  getChannel :: Text
} deriving (Eq, Ord)

instance FromJSON Channel where
  parseJSON = fmap Channel . parseJSON

instance FromJSON EventWrapper where
  parseJSON v = withObject "EventWrapper" (\v' ->
    v' .: "type" >>= \tpe -> case tpe of
      "url_verification" -> EventWrapperChallenge <$> parseJSON v
      "event_callback"   -> EventWrapperEvent <$> parseJSON v
      _                  -> fail $ "Invalid event wrapper type: " ++ (Text.unpack tpe)
    ) v

newtype Token = Token {
  getToken :: Text
} deriving (Eq, Ord)

instance FromJSON Token where
  parseJSON = fmap Token . parseJSON

data ChallengeRequest = ChallengeRequest {
  challengeRequestToken :: Token
, challengeRequestChallengeString :: ChallengeString
} deriving (Eq, Ord)

instance FromJSON ChallengeRequest where
  parseJSON = withObject "ChallengeRequest" $ \v -> ChallengeRequest
    <$> v .: "token"
    <*> v .: "challenge"

newtype ChallengeResponse = ChallengeResponse {
  challengeResponseChallengeString :: ChallengeString
} deriving (Eq, Ord)

instance ToJSON ChallengeResponse where
  toJSON r = object [
      "challenge" .= challengeResponseChallengeString r
    ]

newtype ChallengeString = ChallengeString {
  getChallengeString :: Text
} deriving (Eq, Ord)

instance FromJSON ChallengeString where
  parseJSON = fmap ChallengeString . parseJSON

instance ToJSON ChallengeString where
  toJSON = toJSON . getChallengeString

parrotApplication :: Token -> Application
parrotApplication token request respond =
  if requestMethod request == methodPost
    then do
      body <- lazyRequestBody request
      case eitherDecode body of
        Right e  -> respond $ parrotBot token e
        Left err -> respond $ responseLBS badRequest400 [] $ BLC.pack err
    else respond $ responseLBS methodNotAllowed405 [] ""

parrotBot :: Token -> EventWrapper -> Response
parrotBot t (EventWrapperChallenge r) = challengeEvent t r

challengeEvent :: Token -> ChallengeRequest -> Response
challengeEvent t r =
  let requestToken    = challengeRequestToken r
      challengeString = challengeRequestChallengeString r
      response        = ChallengeResponse challengeString
  in  if requestToken == t
    then responseLBS ok200        [(hContentType, "application/json")] (encode response)
    else responseLBS forbidden403 [] ""

