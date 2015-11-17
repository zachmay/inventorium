module Types.Model.CheckIn (
    CheckIn(..),
    CheckInDetail(..),
    CheckInExpand(..),
    CheckInId(..),
    CheckInSortBy(..)
) where

import Control.Applicative    ((<$>))
import Data.Aeson             (Value(..), FromJSON, ToJSON, fromJSON, toJSON)
import Data.Maybe             (isNothing)
import Data.Text              (pack, unpack)
import Database.Persist       (Entity)
import Database.Persist.Sql   (fromSqlKey, toSqlKey)
import Servant                (FromText, ToText, fromText, toText)
import Text.Read              (readMaybe)

import JSONUtil               (maybeUpdateWithAll)
import Types.Model.Persistent

instance ToText CheckInId where
    toText k = pack . show . fromSqlKey $ k

instance FromText CheckInId where
    fromText t = toSqlKey <$> (readMaybe . unpack $ t)

data CheckInDetail = CheckInDetail { checkIn :: Entity CheckIn
                                   , room :: Maybe (Entity Room)
                                   , user :: Maybe (Entity User) }

instance ToJSON CheckInDetail where
    toJSON (CheckInDetail { checkIn = c, room = r, user = u }) =
        maybeUpdateWithAll (toJSON c) [ ("room", toJSON <$> r)
                                      , ("user", toJSON <$> u) ]

{- Check-in 'sort-by' options -}

data CheckInSortBy = CheckInSortByDate
                   deriving (Eq, Ord, Show)

instance ToText CheckInSortBy where
    toText CheckInSortByDate = "date"

instance FromText CheckInSortBy where
    fromText "date" = Just CheckInSortByDate
    fromText _      = Nothing

{- Check-in 'expand' options -}

data CheckInExpand = CheckInExpandUser
                   | CheckInExpandRoom
                   deriving (Eq, Ord, Show)

instance ToText CheckInExpand where
    toText CheckInExpandRoom = "room"
    toText CheckInExpandUser = "user"

instance FromText CheckInExpand where
    fromText "room" = Just CheckInExpandRoom
    fromText "user" = Just CheckInExpandUser
    fromText _      = Nothing

