module Api.Handlers.Inventory where

import Control.Monad (when)
import Control.Monad.Trans.Either (EitherT, left)
import Data.Maybe (isNothing)
import Servant
import Api.Types.Facilities
import Data.Text (Text)
import Data.ByteString (append)
import Control.Monad.IO.Class (liftIO)
import Auth
import Util (unimplemented)
import Models
import Types
import Errors
import Queries.Facilities
import Database.Persist.Types (Entity(..), Filter, SelectOpt)
import Database.Persist.Class (count, delete, get, getBy, insert, replace, selectFirst, selectList)
import Database.Persist ((==.), (!=.))

-- | Handles HTTP GET for the item type collection.
getItemTypeList :: Maybe AuthToken -> Handler [Entity ItemType]
getItemTypeList auth = do
    checkAuthToken auth
    runDb $ selectList [] []

-- | Handles HTTP POST for the item type collection.
postItemTypeList :: Maybe AuthToken -> ItemType -> Handler (Entity ItemType)
postItemTypeList auth itemType = do
    checkAuthToken auth 
    validateItemType Nothing itemType
    itemTypeId <- runDb $ insert itemType
    return $ Entity itemTypeId itemType

-- | Handles HTTP GET for individual item type resources.
-- TODO: Get list of fields for this item type
getItemType :: ItemTypeId -> Maybe AuthToken -> Handler (Entity ItemType)
getItemType itemTypeId auth = do
    checkAuthToken auth
    itemType <- fetchItemTypeOr404 itemTypeId
    return $ Entity itemTypeId itemType

-- | Handles HTTP PUT for individual item type resources.
putItemType :: ItemTypeId -> Maybe AuthToken -> ItemType -> Handler (Entity ItemType)
putItemType itemTypeId auth itemType = do
    checkAuthToken auth
    fetchItemTypeOr404 itemTypeId
    validateItemType (Just itemTypeId) itemType
    runDb $ replace itemTypeId itemType
    return $ Entity itemTypeId itemType

-- | Handles HTTP DELETE for individual item type resources.
deleteItemType :: ItemTypeId -> Maybe AuthToken -> Handler ()
deleteItemType itemTypeId auth = do
    fetchItemTypeOr404 itemTypeId
    itemCount <- runDb $ count [ItemItemType ==. itemTypeId]
    when (itemCount > 0) $ do
        fail400 "Item types that are in use can not be deleted."
    runDb $ delete itemTypeId
    return ()

-- | Handles HTTP GET for the master inventory list collection.
-- TODO: Honor ItemSortBy
-- TODO: Honor ItemExpand
getMasterInventory :: Maybe AuthToken -> Maybe ItemSortBy -> [ItemExpand] -> Handler [Entity Item]
getMasterInventory auth sort expand = do
    runDb $ selectList [] []

-- | Handles HTTP POST for the master inventory list collection.
postMasterInventory :: Maybe AuthToken -> Item -> Handler (Entity Item)
postMasterInventory auth item = failNotImplemented

getBuildingInventory :: BuildingId -> Maybe AuthToken -> Maybe ItemSortBy -> [ItemExpand] -> Handler [Entity Item]
getBuildingInventory bid auth sort expand = failNotImplemented

getRoomInventory :: BuildingId -> RoomId -> Maybe AuthToken -> Maybe ItemSortBy -> [ItemExpand] -> Handler [Entity Item]
getRoomInventory bid rid auth sort expand = failNotImplemented

postRoomInventory :: BuildingId -> RoomId -> Maybe AuthToken -> Item -> Handler (Entity Item)
postRoomInventory bid rid auth item = failNotImplemented

{- Individual item resources -}

-- TODO: Implement
getItem :: ItemId -> Maybe AuthToken -> [ItemExpand] -> Handler ItemDetail
getItem iid auth expand = do
    checkAuthToken auth
    [(item, maybeCheckIn)] <- runDb $ getItemCurrentCheckIn iid
    return $ ItemDetail { item = item, currentCheckIn = maybeCheckIn }

putItem :: ItemId -> Maybe AuthToken -> Item -> Handler (Entity Item)
putItem iid auth item = failNotImplemented

deleteItem :: ItemId -> Maybe AuthToken -> Handler ()
deleteItem iid auth = failNotImplemented

{- Item check-in history collection -}

getItemHistory :: ItemId -> Maybe AuthToken -> Maybe CheckInSortBy -> [CheckInExpand] -> Handler [Entity CheckIn]
getItemHistory iid auth sort expand = failNotImplemented

postItemHistory :: ItemId -> Maybe AuthToken -> CheckIn -> Handler (Entity CheckIn)
postItemHistory iid auth checkin = failNotImplemented

{- Item check-in resources -}

getItemLatestCheckIn :: ItemId -> Maybe AuthToken -> [CheckInExpand] -> Handler (Entity CheckIn)
getItemLatestCheckIn iid auth expand = failNotImplemented

getItemCheckIn :: ItemId -> CheckInId -> Maybe AuthToken -> [CheckInExpand] -> Handler (Entity CheckIn)
getItemCheckIn iid cid auth expand = failNotImplemented


-- | Helper functions

-- | Attempts to find an item type with the given ID, failing if none is found.
fetchItemTypeOr404 :: ItemTypeId -> Handler ItemType
fetchItemTypeOr404 itemTypeId = do
    maybeItemType <- runDb $ get itemTypeId
    case maybeItemType of
        Nothing       -> fail404 "Item type not found."
        Just itemType -> return itemType

-- | Validates item type details.
-- Can be called with `itemTypeId == Nothing` when creating a new item type record or
-- `itemTypeId = Just _` so that we can filter out the existing record when 
-- checking for duplicate item type names.
--
-- Validations:
--   * Item type name must not be empty.
--   * The item type's name must not be the same as another item type.
validateItemType :: Maybe ItemTypeId -> ItemType -> Handler ()
validateItemType itemTypeId itemType = do
    let name = itemTypeName itemType
        query = case itemTypeId of
                    Nothing  -> getBy $ UniqueItemTypeName name
                    Just iid -> selectFirst [ItemTypeName ==. name, ItemTypeId !=. iid] []

    -- The item type name must not be empty.
    when (name == "") $ do
        fail400 "Item type name may not be empty."

    -- The item type name must not be a duplicate.
    itemTypeByName <- runDb query
    when (not . isNothing $ itemTypeByName) $ do
        fail404 "Item type name already exists."

    return ()

