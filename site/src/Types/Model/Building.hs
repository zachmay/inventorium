module Types.Model.Building (
    Building(..),
    BuildingDetail(..),
    BuildingId(..),
    BuildingExpand(..),
    BuildingSortBy(..)
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

instance ToText BuildingId where
    toText k = pack . show . fromSqlKey $ k

instance FromText BuildingId where
    fromText t = toSqlKey <$> (readMaybe . unpack $ t)

data BuildingDetail = BuildingDetail { building :: Entity Building
                                     , rooms :: Maybe [Entity Room] }

instance ToJSON BuildingDetail where
    toJSON (BuildingDetail { building = b, rooms = rs }) =
        maybeUpdateWithAll (toJSON b) [("rooms", toJSON <$> rs)]

data BuildingSortBy = BuildingSortByDateCreated
                    | BuildingSortByDateUpdated
                    | BuildingSortByDescription
                    | BuildingSortByName
                    deriving (Eq, Ord, Show)

instance ToText BuildingSortBy where
    toText BuildingSortByDateCreated = "created"
    toText BuildingSortByDateUpdated = "updated"
    toText BuildingSortByDescription = "description"
    toText BuildingSortByName        = "name"

instance FromText BuildingSortBy where
    fromText "created"     = Just BuildingSortByDateCreated
    fromText "updated"     = Just BuildingSortByDateUpdated
    fromText "description" = Just BuildingSortByDescription
    fromText "name"        = Just BuildingSortByName
    fromText _             = Nothing

{- Building 'expand' options -}

data BuildingExpand = BuildingExpandRooms
                    deriving (Eq, Ord, Show)

instance ToText BuildingExpand where
    toText BuildingExpandRooms = "rooms"

instance FromText BuildingExpand where
    fromText "rooms" = Just BuildingExpandRooms
    fromText _       = Nothing
