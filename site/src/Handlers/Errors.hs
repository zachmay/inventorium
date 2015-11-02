module Handlers.Errors where

import Control.Monad.Reader (lift)
import Control.Monad.Trans.Either (EitherT, left)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (ByteString)
import Servant.Server

import Types.Misc

failWithMessage :: ServantErr -> ByteString -> Handler a
failWithMessage baseError message = failure $ baseError { errBody = message }

fail404 :: ByteString -> Handler a
fail404 = failWithMessage err404

fail400 :: ByteString -> Handler a
fail400 = failWithMessage err400

failNotImplemented :: Handler a
failNotImplemented = failWithMessage err501 "Not implemented."

failure :: ServantErr -> Handler a
failure e = lift $ left e
