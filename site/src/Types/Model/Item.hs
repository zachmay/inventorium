module Types.Model.Item ( Item(..)
                        , ItemExpand(..)
                        , ItemDetail(..)
                        , ItemId(..)
                        , ItemSortBy(..)
) where

import Control.Applicative    ((<$>))
import Data.Aeson             (Value(..), FromJSON, ToJSON, fromJSON, toJSON)
import Data.Text              (pack, unpack)
import Database.Persist       (Entity)
import Database.Persist.Sql   (fromSqlKey, toSqlKey)
import Servant                (FromText, ToText, fromText, toText)
import Text.Read              (readMaybe)

import JSONUtil               (maybeUpdateWithAll)
import Types.Model.Persistent

instance ToText ItemId where
    toText k = pack . show . fromSqlKey $ k

instance FromText ItemId where
    fromText t = toSqlKey <$> (readMaybe . unpack $ t)

{- Item 'sort-by' options -}

data ItemSortBy = ItemSortByDateCreated
                | ItemSortByDateUpdated
                | ItemSortByCheckInDate
                | ItemSortByName
                deriving (Eq, Ord, Show)

instance ToText ItemSortBy where
    toText ItemSortByDateCreated = "created"
    toText ItemSortByDateUpdated = "updated"
    toText ItemSortByCheckInDate = "checkin"
    toText ItemSortByName        = "name"

instance FromText ItemSortBy where
    fromText "created"  = Just ItemSortByDateCreated
    fromText "updated"  = Just ItemSortByDateUpdated
    fromText "checkin"  = Just ItemSortByCheckInDate
    fromText "name"     = Just ItemSortByName
    fromText _          = Nothing

{- Item 'expand' options -}

data ItemExpand = ItemExpandCheckIns
                deriving (Eq, Ord, Show)

instance ToText ItemExpand where
    toText ItemExpandCheckIns = "checkins"

instance FromText ItemExpand where
    fromText "checkins" = Just ItemExpandCheckIns
    fromText _          = Nothing

data ItemDetail = ItemDetail { item :: Entity Item
                             , currentCheckIn :: Maybe (Entity CheckIn) }

instance ToJSON ItemDetail where
    toJSON (ItemDetail { item = i, currentCheckIn = c }) =
        maybeUpdateWithAll (toJSON i) [("currentCheckIn", toJSON <$> c)]
