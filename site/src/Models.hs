module Models where

import Config
import Control.Applicative ((<$>))
import Control.Monad.IO.Class  
import Control.Monad.Reader        (ReaderT, asks, liftIO, lift)
import Control.Monad.Reader.Class  
import Control.Monad.Trans.Either  (left)
import Data.Aeson
import Data.Maybe (isNothing)
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (UTCTime)
import Data.Typeable
import Database.Persist.Postgresql (SqlBackend(..), runMigration, runSqlPool)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.Sql  
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Database.Persist.TH (share, mkPersist, sqlSettings, mkMigrate, persistFileWith)
import GHC.Generics                (Generic)
import Servant
import Servant.API
import Sort
import JSONUtil
import Text.Read (readMaybe)
import Types (Handler)

type Email = Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "src/model")

{- FromText and ToText instances for persistent keys -}

instance ToText BuildingId where
    toText k = pack . show . fromSqlKey $ k

instance FromText BuildingId where
    fromText t = toSqlKey <$> (readMaybe . unpack $ t)

instance ToText RoomId where
    toText k = pack . show . fromSqlKey $ k

instance FromText RoomId where
    fromText t = toSqlKey <$> (readMaybe . unpack $ t)

instance ToText ItemId where
    toText k = pack . show . fromSqlKey $ k

instance FromText ItemId where
    fromText t = toSqlKey <$> (readMaybe . unpack $ t)

instance ToText ItemTypeId where
    toText k = pack . show . fromSqlKey $ k

instance FromText ItemTypeId where
    fromText t = toSqlKey <$> (readMaybe . unpack $ t)

instance ToText CheckInId where
    toText k = pack . show . fromSqlKey $ k

instance FromText CheckInId where
    fromText t = toSqlKey <$> (readMaybe . unpack $ t)

{- Extended Building data types -}


data BuildingDetail = BuildingDetail { building :: Entity Building
                                     , rooms :: Maybe [Entity Room] }

instance ToJSON BuildingDetail where
    toJSON (BuildingDetail { building = b, rooms = rs }) =
        case base of 
            Object kv -> if isNothing rs then base
                                         else base `updateWith` ("rooms", toJSON rs)
        where base = toJSON b



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
        case base of
            Object kv -> base `updateWith` ("currentCheckIn", toJSON c)
        where base = toJSON i

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
                   deriving (Eq, Ord, Show)

instance ToText CheckInExpand where
    toText CheckInExpandUser = "user"

instance FromText CheckInExpand where
    fromText "user" = Just CheckInExpandUser
    fromText _      = Nothing

{- Report data types -}

data ReconciliationReport = ReconciliationReport {}
                          deriving (Generic)

instance ToJSON ReconciliationReport where
    toJSON r = Null

data ByTypeReport = ByTypeReport {}
                  deriving (Generic)

instance ToJSON ByTypeReport where
    toJSON r = Null

{-
 - Database-related helper functions -} 

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

-- runDb :: SqlPersistT IO b -> Handler b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

