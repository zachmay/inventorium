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

getBuildingListR                     = defaultApiHandler
postBuildingListR                    = defaultApiHandler
getBuildingR buildingId              = basicApiHandler $ object [ "building" .= buildingId ]
putBuildingR buildingId              = basicApiHandler $ object [ "building" .= buildingId ]
deleteBuildingR buildingId           = basicApiHandler $ object [ "building" .= buildingId ]
getRoomListR buildingId              = basicApiHandler $ object [ "building" .= buildingId ]
postRoomListR buildingId             = basicApiHandler $ object [ "building" .= buildingId ]
getRoomR buildingId roomId           = basicApiHandler $ object [ "building" .= buildingId, "room" .= roomId ]
putRoomR buildingId roomId           = basicApiHandler $ object [ "building" .= buildingId, "room" .= roomId ]
deleteRoomR buildingId roomId        = basicApiHandler $ object [ "building" .= buildingId, "room" .= roomId ]
getRoomInventoryR buildingId roomId  = basicApiHandler $ object [ "building" .= buildingId, "room" .= roomId ]
postRoomInventoryR buildingId roomId = basicApiHandler $ object [ "building" .= buildingId, "room" .= roomId ]
