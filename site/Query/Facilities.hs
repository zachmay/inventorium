module Query.Facilities where

import Import

getBuildings = do
    runDB $ selectList [] [Asc BuildingName]

buildingExists buildingId = do
    _ <- runDB $ get404 buildingId
    return ()

getBuildingRoomList buildingId = do
    runDB $ selectList [ RoomBuilding ==. buildingId ]
                       [ Asc RoomName ]

getRoomInBuilding buildingId roomId = do
    buildingExists buildingId
    runDB $ get404 roomId




