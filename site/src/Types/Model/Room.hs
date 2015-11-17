module Types.Model.Room ( Room(..)
                        , RoomId(..)
                        , RoomExpand(..)
                        , RoomDetail(..)
                        , RoomSortBy(..)
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

instance ToText RoomId where
    toText k = pack . show . fromSqlKey $ k

instance FromText RoomId where
    fromText t = toSqlKey <$> (readMaybe . unpack $ t)

data RoomDetail = RoomDetail { room :: Entity Room
                             , inventory :: Maybe [Entity Item] }

instance ToJSON RoomDetail where
    toJSON (RoomDetail { room = r, inventory = is }) =
        maybeUpdateWithAll (toJSON r) [("inventory", toJSON <$> map toJSON <$> is)]

{- Room 'sort-by' options -}

data RoomSortBy = RoomSortByDateCreated
                | RoomSortByDateUpdated
                | RoomSortByName
                | RoomSortByDescription
                deriving (Eq, Ord, Show)

instance ToText RoomSortBy where
    toText RoomSortByDateCreated = "created"
    toText RoomSortByDateUpdated = "updated"
    toText RoomSortByName        = "name"
    toText RoomSortByDescription = "description"

instance FromText RoomSortBy where
    fromText "created"     = Just RoomSortByDateCreated
    fromText "updated"     = Just RoomSortByDateUpdated
    fromText "name"        = Just RoomSortByName
    fromText "description" = Just RoomSortByDescription
    fromText _             = Nothing

{- Room 'expand' options -}

data RoomExpand = RoomExpandInventory
                deriving (Eq, Ord, Show)

instance ToText RoomExpand where
    toText RoomExpandInventory = "inventory"

instance FromText RoomExpand where
    fromText "inventory" = Just RoomExpandInventory
    fromText _           = Nothing

