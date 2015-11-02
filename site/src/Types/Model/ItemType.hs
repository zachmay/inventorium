module Types.Model.ItemType ( ItemType(..)
                            , ItemTypeId(..)
                            , ItemTypeDetail(..)
                            , ItemTypeExpand(..)
) where

import Data.Aeson             (Value(..), FromJSON, ToJSON, fromJSON, toJSON)
import Data.Maybe             (isNothing)
import Data.Text              (pack, unpack)
import Database.Persist       (Entity)
import Database.Persist.Sql   (fromSqlKey, toSqlKey)
import Servant                (FromText, ToText, fromText, toText)
import Text.Read              (readMaybe)

import JSONUtil               (maybeUpdateWithAll)
import Types.Model.Persistent

instance ToText ItemTypeId where
    toText k = pack . show . fromSqlKey $ k

instance FromText ItemTypeId where
    fromText t = toSqlKey <$> (readMaybe . unpack $ t)

{- Extended data types for Item Types -}
    
data ItemTypeDetail = ItemTypeDetail { itemType :: Entity ItemType
                                     , properties :: Maybe [Entity ItemTypeProperty] }

instance ToJSON ItemTypeDetail where
    toJSON (ItemTypeDetail { itemType = it, properties = props }) =
        maybeUpdateWithAll (toJSON it) [("properties", toJSON <$> props)]

data ItemTypeExpand = ItemTypeExpandProperties
                    deriving (Eq, Ord, Show)

instance ToText ItemTypeExpand where
    toText ItemTypeExpandProperties = "properties"

instance FromText ItemTypeExpand where
    fromText "properties" = Just ItemTypeExpandProperties
    fromText _            = Nothing

