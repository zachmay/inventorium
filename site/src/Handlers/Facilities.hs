module Handlers.Facilities (facilitiesHandlers) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Trans.Either (EitherT, left)
import Data.Maybe (isNothing)
import Data.Map (empty, findWithDefault)
import Data.Time.Clock (getCurrentTime)
import Servant
import Data.Text (Text)
import Data.ByteString (append)
import Control.Monad.IO.Class (liftIO)
import Queries.Facilities
import Util (unimplemented, groupMap)
import Database.Persist.Types (Entity(..), Filter, SelectOpt(..))
import Database.Persist.Class (count, delete, get, getBy, insert, replace, selectFirst, selectList)
import Database.Persist ((==.), (!=.), (<-.))

import Handlers.Auth
import Handlers.Errors
import Types.Api.Facilities
import Types.Misc
import Types.Sort
import Types.Model.Persistent
import Types.Model.Building
import Types.Model.Room

-----------------------------------------------------------------------------

facilitiesHandlers :: ServerT FacilitiesApi Handler
facilitiesHandlers = getBuildingList
                :<|> postBuildingList
                :<|> getBuilding
                :<|> putBuilding
                :<|> deleteBuilding
                :<|> getRoomList
                :<|> postRoomList
                :<|> getRoom
                :<|> putRoom
                :<|> deleteRoom

-----------------------------------------------------------------------------

-- | Building collection
 
-- | Handles HTTP GET for the building collection.
getBuildingList :: Maybe AuthToken -> [SortField BuildingSortBy] -> [BuildingExpand] -> Handler [BuildingDetail]
getBuildingList auth sortBy expand = do
    checkAuthToken auth 
    buildings <- runDb $ selectList [] (map sortByToQueryOption sortBy)
    roomMap <- fetchRoomMap $ map entityKey buildings
    return $ map (gatherBuildingDetails roomMap) buildings
    where sortByToQueryOption (SortField Ascending  BuildingSortByDateCreated) = Asc  BuildingCreated
          sortByToQueryOption (SortField Ascending  BuildingSortByDateUpdated) = Asc  BuildingUpdated
          sortByToQueryOption (SortField Ascending  BuildingSortByDescription) = Asc  BuildingDescription
          sortByToQueryOption (SortField Ascending  BuildingSortByName)        = Asc  BuildingName
          sortByToQueryOption (SortField Descending BuildingSortByDateCreated) = Desc BuildingCreated
          sortByToQueryOption (SortField Descending BuildingSortByDateUpdated) = Desc BuildingUpdated
          sortByToQueryOption (SortField Descending BuildingSortByDescription) = Desc BuildingDescription
          sortByToQueryOption (SortField Descending BuildingSortByName)        = Desc BuildingName

          gatherBuildingDetails roomMap b =
              BuildingDetail { building = b
                             , rooms = if BuildingExpandRooms `elem` expand
                                       then Just $ findWithDefault [] (entityKey b) roomMap
                                       else Nothing }

          fetchRoomMap buildingIds = if BuildingExpandRooms `elem` expand 
                                     then do
                                        rooms <- runDb $ selectList [RoomBuilding <-. buildingIds] []
                                        return $ groupMap (roomBuilding . entityVal) rooms
                                     else return empty

-- | Handles HTTP POST for the building collection.
postBuildingList :: Maybe AuthToken -> Building -> Handler (Entity Building)
postBuildingList auth building = do
    checkAuthToken auth
    validateBuilding Nothing building
    buildingId <- runDb $ insert building
    return $ Entity buildingId building

-----------------------------------------------------------------------------

-- | Building resources

-- | Handles HTTP GET for individual building resources.
getBuilding ::  BuildingId -> Maybe AuthToken -> [BuildingExpand] -> Handler BuildingDetail
getBuilding buildingId auth expand = do
    checkAuthToken auth
    building <- fetchBuildingOr404 buildingId
    rooms <- fetchRooms
    return $ BuildingDetail { building = Entity buildingId building, rooms = rooms }
        where fetchRooms = if BuildingExpandRooms `elem` expand
                           then do
                               rooms <- runDb $ selectList [RoomBuilding ==. buildingId] []
                               return $ Just rooms
                           else return Nothing

-- | Handles HTTP PUT for individual building resources.
putBuilding ::  BuildingId -> Maybe AuthToken -> Building -> Handler (Entity Building)
putBuilding buildingId auth building = do
    checkAuthToken auth
    existing <- fetchBuildingOr404 buildingId
    now <- liftIO $ getCurrentTime
    let building' = building { buildingUpdated = now }
    validateBuilding (Just buildingId) building'
    runDb $ replace buildingId building'
    return $ Entity buildingId building'

-- | Handles HTTP DELETE for individual building resources.
--
-- Validations: 
--   * The specified building must exist. (404)
--   * No rooms should be attached to the building. (400)
deleteBuilding ::  BuildingId -> Maybe AuthToken -> Handler ()
deleteBuilding buildingId auth = do
    checkAuthToken auth
    existing <- fetchBuildingOr404 buildingId
    roomCount <- runDb $ count [RoomBuilding ==. buildingId]
    when (roomCount > 0) $ do
        fail400 "Buildings with attached rooms can not be deleted."
    runDb $ delete buildingId
    return ()

-----------------------------------------------------------------------------

-- | Building room list collection

-- | Handles HTTP GET for a building's room collection.
-- TODO: Honor RoomExpand
getRoomList :: BuildingId -> Maybe AuthToken -> [SortField RoomSortBy] -> [RoomExpand] -> Handler [Entity Room]
getRoomList buildingId auth sortBy expand = do
    checkAuthToken auth 
    building <- fetchBuildingOr404 buildingId
    rooms <- runDb $ selectList [RoomBuilding ==. buildingId] (map sortByToQueryOption sortBy)
    return rooms
    where sortByToQueryOption (SortField Ascending  RoomSortByDateCreated) = Asc  RoomCreated
          sortByToQueryOption (SortField Ascending  RoomSortByDateUpdated) = Asc  RoomUpdated
          sortByToQueryOption (SortField Ascending  RoomSortByDescription) = Asc  RoomDescription
          sortByToQueryOption (SortField Ascending  RoomSortByName)        = Asc  RoomName
          sortByToQueryOption (SortField Descending RoomSortByDateCreated) = Desc RoomCreated
          sortByToQueryOption (SortField Descending RoomSortByDateUpdated) = Desc RoomUpdated
          sortByToQueryOption (SortField Descending RoomSortByDescription) = Desc RoomDescription
          sortByToQueryOption (SortField Descending RoomSortByName)        = Desc RoomName

-- | Handles HTTP POST for a building's room collection.
postRoomList :: BuildingId -> Maybe AuthToken -> Room -> Handler (Entity Room)
postRoomList buildingId auth room = do
    let name = roomName room
    checkAuthToken auth
    validateRoom buildingId Nothing room
    roomId <- runDb $ insert room
    return $ Entity roomId room

-- | Room resources

-- | Handles HTTP GET for individual room resources.
-- TODO: Honor RoomExpand
getRoom :: BuildingId -> RoomId -> Maybe AuthToken -> [RoomExpand] -> Handler (Entity Room)
getRoom buildingId roomId auth expand = do
    checkAuthToken auth
    (building, room) <- fetchBuildingRoomOr404 buildingId roomId
    return $ Entity roomId room

-- | Handles HTTP PUT for individual room resources.
putRoom :: BuildingId -> RoomId -> Maybe AuthToken -> Room -> Handler (Entity Room)
putRoom buildingId roomId auth room = do
    checkAuthToken auth
    validateRoom buildingId (Just roomId) room
    runDb $ replace roomId room
    return $ Entity roomId room

-- | Handles HTTP DELETE for individual room resources.
-- TODO: Validation - Disallow if room contains inventory
deleteRoom :: BuildingId -> RoomId -> Maybe AuthToken -> Handler ()
deleteRoom buildingId roomId auth = do
    checkAuthToken auth
    (building, room) <- fetchBuildingRoomOr404 buildingId roomId
    runDb $ delete roomId
    return ()

-- | Helper functions

-- | Attempts to find a building with the given BuildingId, failing with 404 if none was foudn.
fetchBuildingOr404 :: BuildingId -> Handler Building
fetchBuildingOr404 buildingId = do
    maybeBuilding <- runDb $ get buildingId
    case maybeBuilding of
        Nothing -> fail404 "Building not found."
        Just b  -> return b

-- | Attempts to find a building and room with the given IDs, failing if either is not found
-- or if the room's actual building ID doesn't match the given building ID.
fetchBuildingRoomOr404 :: BuildingId -> RoomId -> Handler (Building, Room)
fetchBuildingRoomOr404 buildingId roomId = do
    building <- fetchBuildingOr404 buildingId
    maybeRoom <- runDb $ get roomId
    case maybeRoom of
        Nothing   -> fail404 "Room not found."
        Just room -> if roomBuilding room /= buildingId
                     then fail404 "Room not found in building."
                     else return (building, room)

-- | Validates the building details.
-- Can be called with `buildingId == Nothing` when creating a new building record or
-- `buildingId == Just _` so that we can filter out the existing record when 
-- checking for duplicate building names.
--
-- Validations:
--   * The building's name must not be empty.
--   * The building's name must not be the same as an existing building's name.
validateBuilding :: Maybe BuildingId -> Building -> Handler ()
validateBuilding buildingId building = do
    let name = buildingName building
        query = case buildingId of
                    Nothing  -> getBy $ UniqueBuildingName name
                    Just bid -> selectFirst [BuildingName ==. name, BuildingId !=. bid] []

    -- The building name must not be empty.
    when (name == "") $ do
        fail400 "Building name may not be empty."

    -- The building name must not be a duplicate.
    buildingByName <- runDb query
    when (not . isNothing $ buildingByName) $ do
        fail404 "Building name already exists."

    return ()

-- | Validates room details.
-- Can be called with `roomId == Nothing` when creating a new room record or
-- `roomId = Just _` so that we can filter out the existing record when 
-- checking for duplicate room names within the building.
--
-- Validations:
--   * Room name must not be empty.
--   * If given a room ID, both the specified building and room must exist and the
--     the room must belong to the building. Otherwise, the building at least needs
--     to exist.
--   * The room's name must not be the same as another room in the same building.
validateRoom :: BuildingId -> Maybe RoomId -> Room -> Handler ()
validateRoom buildingId roomId room = do
    let name = (roomName room)
        query = selectFirst filters []
            where filters = [RoomBuilding ==. buildingId, RoomName ==. name]
                                ++ case roomId of
                                       Nothing  -> []
                                       Just rid -> [RoomId !=. rid]

    -- Room name must not be empty.
    when (name == "") $ do
        fail400 "Room name cannot be empty."

    -- The given building must exist and if the room ID is specified,
    -- the room should belong to the building.
    case roomId of
        Nothing  -> fetchBuildingOr404 buildingId         >> return ()
        Just rid -> fetchBuildingRoomOr404 buildingId rid >> return ()

    -- The room name must not be a duplicate.
    roomByName <- runDb query
    when (not . isNothing $ roomByName) $ do
        fail404 "Room name already exists in this building."

    return ()

