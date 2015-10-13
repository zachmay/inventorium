module Models where

import Database.Persist.TH (share, mkPersist, sqlSettings, mkMigrate, persistFileWith)
import Database.Persist.Quasi (lowerCaseSettings)
import Data.Text (Text, pack, unpack)
import Text.Read (readMaybe)
import Data.Time.Clock (UTCTime)
import Data.Typeable
import Data.Aeson                  (ToJSON, FromJSON)
import GHC.Generics                (Generic)
import Control.Monad.Reader        (ReaderT, asks, liftIO, lift)
import Control.Monad.Trans.Either  (left)
import Control.Monad.Reader.Class  
import Control.Monad.IO.Class  
import Control.Applicative ((<$>))
import Database.Persist.Postgresql (SqlBackend(..), runMigration, runSqlPool)
import Database.Persist.Sql  
import Config
import Types (Handler)
import Servant.API
import Servant
import Database.Persist.Sql (fromSqlKey, toSqlKey)

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

{- Building sort-by options -}

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

{- Building 'expand' options -}

data BuildingExpand = BuildingExpandRooms
                    deriving (Eq, Ord, Show)

instance ToText BuildingExpand where
    toText BuildingExpandRooms = "rooms"

instance FromText BuildingExpand where
    fromText "rooms" = Just BuildingExpandRooms

{- Room 'sort-by' options -}

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

{- Room 'expand' options -}

data RoomExpand = RoomExpandInventory
                deriving (Eq, Ord, Show)

instance ToText RoomExpand where
    toText RoomExpandInventory = "inventory"

instance FromText RoomExpand where
    fromText "inventory" = Just RoomExpandInventory

{- Item 'sort-by' options -}

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

{- Item 'expand' options -}

data ItemExpand = ItemExpandCheckIns
                deriving (Eq, Ord, Show)

instance ToText ItemExpand where
    toText ItemExpandCheckIns = "checkins"

instance FromText ItemExpand where
    fromText "checkins" = Just ItemExpandCheckIns

{- Check-in 'sort-by' options -}

data CheckInSortBy = CheckInSortByDate
                   deriving (Eq, Ord, Show)

instance ToText CheckInSortBy where
    toText CheckInSortByDate = "date"

instance FromText CheckInSortBy where
    fromText "date" = Just CheckInSortByDate

{- Check-in 'expand' options -}

data CheckInExpand = CheckInExpandUser
                   deriving (Eq, Ord, Show)

instance ToText CheckInExpand where
    toText CheckInExpandUser = "user"

instance FromText CheckInExpand where
    fromText "user" = Just CheckInExpandUser

{- Report data types -}

data ReconciliationReport = ReconciliationReport {}
                          deriving (Generic)

instance ToJSON ReconciliationReport

data ByTypeReport = ByTypeReport {}
                  deriving (Generic)

instance ToJSON ByTypeReport

{-
 - Database-related helper functions -} 

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

-- runDb :: SqlPersistT IO b -> Handler b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

