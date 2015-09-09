module Handler.Api where

import Import

defaultApiHandler :: Handler TypedContent
defaultApiHandler = selectRep $ do
    provideRep $ return $ object []

basicApiHandler :: Value -> Handler TypedContent
basicApiHandler obj = selectRep $ do
    provideRep $ return $ obj

getInventoryR, postInventoryR :: Handler TypedContent
getInventoryR                      = defaultApiHandler
postInventoryR                     = defaultApiHandler

getItemR, putItemR, deleteItemR :: ItemId -> Handler TypedContent
getItemR         itemId            = basicApiHandler $ object [ "item" .= itemId ]
putItemR         itemId            = basicApiHandler $ object [ "item" .= itemId ]
deleteItemR      itemId            = basicApiHandler $ object [ "item" .= itemId ]

getItemHistoryR, postItemHistoryR :: ItemId -> Handler TypedContent
getItemHistoryR  itemId            = basicApiHandler $ object [ "item" .= itemId ]
postItemHistoryR itemId            = basicApiHandler $ object [ "item" .= itemId ]

getItemCheckInR :: ItemId -> CheckInId -> Handler TypedContent
getItemCheckInR  itemId checkInId  = basicApiHandler $ object [ "item" .= itemId, "checkIn" .= checkInId ]

getReportReconciliationR, getReportByTypeR :: Handler TypedContent
getReportReconciliationR           = defaultApiHandler

getReportByTypeR                   = defaultApiHandler

getBuildingListR :: Handler TypedContent
getBuildingListR = do
    buildingEntities <- runDB $ selectList [] [Asc BuildingName]
    basicApiHandler $ toJSON buildingEntities

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
    basicApiHandler . toJSON $ Entity buildingId building

putBuildingR :: BuildingId -> Handler TypedContent
putBuildingR buildingId = 
    basicApiHandler $ object [ "building" .= buildingId ]

deleteBuildingR :: BuildingId -> Handler TypedContent
deleteBuildingR buildingId =
    basicApiHandler $ object [ "building" .= buildingId ]

getRoomListR :: BuildingId -> Handler TypedContent
getRoomListR buildingId =
    basicApiHandler $ object [ "building" .= buildingId ]

postRoomListR :: BuildingId -> Handler TypedContent
postRoomListR buildingId =
    basicApiHandler $ object [ "building" .= buildingId ]

getRoomR :: BuildingId -> RoomId -> Handler TypedContent
getRoomR buildingId roomId =
    basicApiHandler $ object [ "building" .= buildingId, "room" .= roomId ]

putRoomR :: BuildingId -> RoomId -> Handler TypedContent
putRoomR buildingId roomId =
    basicApiHandler $ object [ "building" .= buildingId, "room" .= roomId ]

deleteRoomR :: BuildingId -> RoomId -> Handler TypedContent
deleteRoomR buildingId roomId =
    basicApiHandler $ object [ "building" .= buildingId, "room" .= roomId ]

getRoomInventoryR :: BuildingId -> RoomId -> Handler TypedContent
getRoomInventoryR buildingId roomId =
    basicApiHandler $ object [ "building" .= buildingId, "room" .= roomId ]

postRoomInventoryR :: BuildingId -> RoomId -> Handler TypedContent
postRoomInventoryR buildingId roomId =
    basicApiHandler $ object [ "building" .= buildingId, "room" .= roomId ]
