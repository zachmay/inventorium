module JSONUtil where

import Data.Aeson          (Value(..))
import Data.HashMap.Strict (insert)
import Data.Text           (Text)

-- | Takes a JSON value (objects only) and updates it with
-- the given key/value pair. Will overwrite existing keys.
updateWith :: Value -> (Text, Value) -> Value
updateWith (Object kvs) (k, v) = Object $ insert k v kvs
updateWith v _ = v

-- | Takes a JSON value (objects only) and updates it with 
-- the given list of key/maybe value pairs. Will overwrite existing
-- keys, keys with Nothing values make no change to the output
-- JSON.
maybeUpdateWithAll :: Value -> [(Text, Maybe Value)] -> Value
maybeUpdateWithAll initial@(Object _) pairs = foldr maybeUpdate initial pairs
    where maybeUpdate (key, maybeValue) obj@(Object kvs) =
              case maybeValue of 
                  Nothing  -> obj
                  Just val -> Object $ insert key val kvs
maybeUpdateWithAll v _ = v
