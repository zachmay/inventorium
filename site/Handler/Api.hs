module Handler.Api where

import Import
import Query.Facilities as Facilities
import Query.Inventory  as Inventory
import Query.Reports    as Reports

defaultApiHandler :: Handler TypedContent
defaultApiHandler = selectRep $ do
    provideRep $ return $ object []

apiHandler :: Value -> Handler TypedContent
apiHandler obj = selectRep $ do
    provideRep $ return $ obj


{-
 - Inventory management 
 -}


getInventoryR :: Handler TypedContent
getInventoryR = do
    inventory <- Inventory.getInventoryList
    apiHandler $ toJSON inventory

postInventoryR :: Handler TypedContent
postInventoryR = do
    item <- requireJsonBody :: Handler Item
    itemId <- runDB $ insert item
    sendResponseStatus created201 . toJSON $ Entity itemId item
    

getItemR :: ItemId -> Handler TypedContent
getItemR itemId = do
    apiHandler $ object [ "item" .= itemId ]

putItemR :: ItemId -> Handler TypedContent
putItemR itemId = do
    apiHandler $ object [ "item" .= itemId ]

deleteItemR :: ItemId -> Handler TypedContent
deleteItemR itemId = do
    apiHandler $ object [ "item" .= itemId ]

getItemHistoryR, postItemHistoryR :: ItemId -> Handler TypedContent
getItemHistoryR  itemId            = apiHandler $ object [ "item" .= itemId ]
postItemHistoryR itemId            = apiHandler $ object [ "item" .= itemId ]

getItemCheckInR :: ItemId -> CheckInId -> Handler TypedContent
getItemCheckInR  itemId checkInId  = apiHandler $ object [ "item" .= itemId, "checkIn" .= checkInId ]

{- 
 - Reports
 -}

getReportReconciliationR, getReportByTypeR :: Handler TypedContent
getReportReconciliationR           = defaultApiHandler

getReportByTypeR                   = defaultApiHandler

{-
 - Facilities Management
 -}
getBuildingListR :: Handler TypedContent
getBuildingListR = do
    buildingEntities <- Facilities.getBuildings
    apiHandler $ toJSON buildingEntities

{- TODO: Handle duplicate name -}
{- TODO: Content negotiation, on both request and response -}
postBuildingListR :: Handler TypedContent
postBuildingListR = do
    building <- requireJsonBody :: Handler Building
    buildingId <- runDB $ insert building
    sendResponseStatus created201 . toJSON $ Entity buildingId building

getBuildingR :: BuildingId -> Handler TypedContent
getBuildingR buildingId = do
    building <- runDB $ get404 buildingId
    apiHandler . toJSON $ Entity buildingId building

putBuildingR :: BuildingId -> Handler TypedContent
putBuildingR buildingId = 
    apiHandler $ object [ "building" .= buildingId ]

deleteBuildingR :: BuildingId -> Handler TypedContent
deleteBuildingR buildingId =
    apiHandler $ object [ "building" .= buildingId ]

getRoomListR :: BuildingId -> Handler TypedContent
getRoomListR buildingId = do
    Facilities.buildingExists buildingId
    rooms <- getBuildingRoomList buildingId
    apiHandler $ array $ map toJSON rooms

postRoomListR :: BuildingId -> Handler TypedContent
postRoomListR buildingId = do
    Facilities.buildingExists buildingId
    room <- requireJsonBody :: Handler Room
    roomId <- runDB $ insert room
    sendResponseStatus created201 . toJSON $ Entity roomId room

getRoomR :: BuildingId -> RoomId -> Handler TypedContent
getRoomR buildingId roomId = do
    room <- getRoomInBuilding buildingId roomId
    apiHandler . toJSON $ room

putRoomR :: BuildingId -> RoomId -> Handler TypedContent
putRoomR buildingId roomId =
    apiHandler $ object [ "building" .= buildingId, "room" .= roomId ]

deleteRoomR :: BuildingId -> RoomId -> Handler TypedContent
deleteRoomR buildingId roomId =
    apiHandler $ object [ "building" .= buildingId, "room" .= roomId ]

getRoomInventoryR :: BuildingId -> RoomId -> Handler TypedContent
getRoomInventoryR buildingId roomId =
    apiHandler $ object [ "building" .= buildingId, "room" .= roomId ]

postRoomInventoryR :: BuildingId -> RoomId -> Handler TypedContent
postRoomInventoryR buildingId roomId =
    apiHandler $ object [ "building" .= buildingId, "room" .= roomId ]
