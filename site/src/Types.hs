module Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Text    (Text)
import GHC.Generics
import Servant.API
import Servant (ServantErr)
import Control.Monad.Reader         (ReaderT, runReaderT, lift)
import Control.Monad.Trans.Either (EitherT)
import Database.Persist.TH
import Database.Esqueleto
import Config

type AuthToken = Text

type Authorized = Header "Authorization" AuthToken

type Handler = ReaderT Config (EitherT ServantErr IO)

type Query a = SqlPersistT IO a

{- Item Property type -}
data PropertyType = FreeText
                  | Numeric
                  deriving (Eq, Generic, Ord, Read, Show)

instance ToJSON PropertyType
instance FromJSON PropertyType

derivePersistField "PropertyType"
