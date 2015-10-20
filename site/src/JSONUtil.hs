module JSONUtil where

import Data.Aeson
import Data.HashMap.Strict
import Data.Text

-- | Takes a JSON value (objects only) and updates it with
-- the given key/value pair. Will overwrite existing keys.
updateWith :: Value -> (Text, Value) -> Value
updateWith (Object kvs) (k, v) = Object $ insert k v kvs
updateWith _ _ = error "updateWith: can only update JSON objects"

