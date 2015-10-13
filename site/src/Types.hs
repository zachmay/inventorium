module Types where

import Data.Aeson
import Data.Text    (Text)
import GHC.Generics
import Servant.API
import Servant (ServantErr)
import Control.Monad.Reader         (ReaderT, runReaderT, lift)
import Control.Monad.Trans.Either (EitherT)
import Config

type AuthToken = Text

type Authorized = Header "Authorization" AuthToken

type Handler = ReaderT Config (EitherT ServantErr IO)
