module Types.Misc where

import Control.Monad.Reader       (ReaderT)
import Control.Monad.Trans.Either (EitherT)
import Data.Aeson                 (ToJSON, toJSON, FromJSON, fromJSON, object, (.=))
import Data.Text                  (Text)
import Database.Persist.Sql       (SqlPersistT)
import Servant                    (Header, ServantErr, FromText, fromText, ToText, toText)
import Data.Text                  (append, intercalate, splitOn, uncons)
import GHC.Generics               (Generic)

import Types.App                  (Config)

type AuthToken = Text

type AuthFailureMessage = Text

type Authorized = Header "Authorization" AuthToken

data AuthRequest = AuthRequest { authUserName :: Text
                               , authPassword :: Text }
                               deriving (Eq, Ord, Show, Generic)

instance FromJSON AuthRequest 
instance ToJSON AuthRequest where

data AuthResponse = AuthSuccess AuthToken
                  | AuthFailure AuthFailureMessage
                  deriving (Eq, Ord, Show)

instance ToJSON AuthResponse where
    toJSON (AuthSuccess token) =
        object [ "result"  .= ("success" :: Text)
               , "token"   .= token ]
    toJSON (AuthFailure message) =
        object [ "result"  .= ("failure" :: Text)
               , "message" .= message ]

type Handler = ReaderT Config (EitherT ServantErr IO)

type Query a = SqlPersistT IO a
