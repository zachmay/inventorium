module Types where

import Data.Aeson
import Data.Text    (Text)
import GHC.Generics
import Servant.API

{- Buildings -}

type BuildingId = Integer
data Building = Building {}
              deriving (Generic)

instance ToJSON Building
instance FromJSON Building

data BuildingSortBy = BuildingSortByDateCreated
                    | BuildingSortByDateModified
                    | BuildingSortByName
                    deriving (Eq, Ord, Show)

instance ToText BuildingSortBy where
    toText BuildingSortByDateCreated  = "created"
    toText BuildingSortByDateModified = "modified"
    toText BuildingSortByName         = "name"

instance FromText BuildingSortBy where
    fromText "created"  = Just BuildingSortByDateCreated
    fromText "modified" = Just BuildingSortByDateModified
    fromText "name"     = Just BuildingSortByName

data BuildingExpand = BuildingExpandRooms
                    deriving (Eq, Ord, Show)

instance ToText BuildingExpand where
    toText BuildingExpandRooms = "rooms"

instance FromText BuildingExpand where
    fromText "rooms" = Just BuildingExpandRooms

{- Rooms -}

type RoomId = Integer
data Room = Room {}
          deriving (Generic)

instance ToJSON Room
instance FromJSON Room

data RoomSortBy = RoomSortByDateCreated
                | RoomSortByDateModified
                | RoomSortByName
                | RoomSortByDescription
                deriving (Eq, Ord, Show)

instance ToText RoomSortBy where
    toText RoomSortByDateCreated     = "created"
    toText RoomSortByDateModified    = "modified"
    toText RoomSortByName            = "name"
    toText RoomSortByDescription     = "description"

instance FromText RoomSortBy where
    fromText "created"     = Just RoomSortByDateCreated
    fromText "modified"    = Just RoomSortByDateModified
    fromText "name"        = Just RoomSortByName
    fromText "description" = Just RoomSortByDescription

data RoomExpand = RoomExpandInventory
                deriving (Eq, Ord, Show)

instance ToText RoomExpand where
    toText RoomExpandInventory = "inventory"

instance FromText RoomExpand where
    fromText "inventory" = Just RoomExpandInventory

{- Items -}

type ItemId = Integer
data Item = Item {}
          deriving (Generic)

instance ToJSON Item
instance FromJSON Item

data ItemSortBy = ItemSortByDateCreated
                | ItemSortByDateModified
                | ItemSortByCheckInDate
                | ItemSortByName
                deriving (Eq, Ord, Show)

instance ToText ItemSortBy where
    toText ItemSortByDateCreated  = "created"
    toText ItemSortByDateModified = "modified"
    toText ItemSortByCheckInDate  = "checkin"
    toText ItemSortByName         = "name"

instance FromText ItemSortBy where
    fromText "created"  = Just ItemSortByDateCreated
    fromText "modified" = Just ItemSortByDateModified
    fromText "checkin"  = Just ItemSortByCheckInDate
    fromText "name"     = Just ItemSortByName

data ItemExpand = ItemExpandCheckIns
                deriving (Eq, Ord, Show)

instance ToText ItemExpand where
    toText ItemExpandCheckIns = "checkins"

instance FromText ItemExpand where
    fromText "checkins" = Just ItemExpandCheckIns

{- Check-ins -}
                
type CheckInId = Integer
data CheckIn = CheckIn {}
             deriving (Generic)

instance ToJSON CheckIn
instance FromJSON CheckIn

data CheckInSortBy = CheckInSortByDate
                   deriving (Eq, Ord, Show)

instance ToText CheckInSortBy where
    toText CheckInSortByDate = "date"

instance FromText CheckInSortBy where
    fromText "date" = Just CheckInSortByDate

data CheckInExpand = CheckInExpandUser
                   deriving (Eq, Ord, Show)

instance ToText CheckInExpand where
    toText CheckInExpandUser = "user"

instance FromText CheckInExpand where
    fromText "user" = Just CheckInExpandUser

{- Reports -}

data ReconciliationReport = ReconciliationReport {}
                          deriving (Generic)

instance ToJSON ReconciliationReport

data ByTypeReport = ByTypeReport {}
                  deriving (Generic)

instance ToJSON ByTypeReport

{- Helpers -}

type AuthToken = Text

type Authorized = Header "Authorization" AuthToken

