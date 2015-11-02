module Types.Model.ItemTypeProperty ( ItemTypeProperty(..)
                                    , ItemTypePropertyId(..)
) where

import Data.Text              (pack, unpack)
import Database.Persist.Sql   (fromSqlKey, toSqlKey)
import Servant                (ToText, toText, FromText, fromText)
import Text.Read              (readMaybe)

import Types.Model.Persistent 

instance ToText ItemTypePropertyId where
    toText k = pack . show . fromSqlKey $ k

instance FromText ItemTypePropertyId where
    fromText t = toSqlKey <$> (readMaybe . unpack $ t)
