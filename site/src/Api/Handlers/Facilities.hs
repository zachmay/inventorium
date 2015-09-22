module Api.Handlers.Facilities where

import Control.Monad.Trans.Either (EitherT, left)
import Servant
import Types
import Api.Types.Facilities
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Auth
import Util (unimplemented)

{- Building collection -}

getBuildingList :: Maybe AuthToken -> Maybe BuildingSortBy -> [BuildingExpand] -> Handler [Building]
getBuildingList auth sortBy expand = do
    checkAuthToken auth 
    return buildings

postBuildingList :: Maybe AuthToken -> Building -> Handler Building
postBuildingList auth building = unimplemented "postBuildingsList"

{- Building resources -}

getBuilding ::  BuildingId -> Maybe AuthToken -> [BuildingExpand] -> Handler Building
getBuilding bId auth expand = return . head $ buildings

putBuilding ::  BuildingId -> Maybe AuthToken -> Building -> Handler Building
putBuilding bId auth building = unimplemented "putBuilding"

patchBuilding ::  BuildingId -> Maybe AuthToken -> Building -> Handler Building
patchBuilding bId auth building = unimplemented "patchBuilding"

deleteBuilding ::  BuildingId -> Maybe AuthToken -> Handler ()
deleteBuilding bId auth = unimplemented "deleteBuilding"

{- Building room list collection -}

getRoomList :: BuildingId -> Maybe AuthToken -> Maybe RoomSortBy -> [RoomExpand] -> Handler [Room]
getRoomList bId auth sortBy expand = return rooms

postRoomList :: BuildingId -> Maybe AuthToken -> Room -> Handler Room
postRoomList bId auth room = undefined

{- Room resources -}

getRoom :: BuildingId -> RoomId -> Maybe AuthToken -> [RoomExpand] -> Handler Room
getRoom bId rId auth expand = return . head $ rooms

putRoom :: BuildingId -> RoomId -> Maybe AuthToken -> Room -> Handler Room
putRoom bId rId auth room = undefined

patchRoom :: BuildingId -> RoomId -> Maybe AuthToken -> Room -> Handler Room
patchRoom bId rId auth room = undefined

deleteRoom :: BuildingId -> RoomId -> Maybe AuthToken -> Handler ()
deleteRoom bId rId auth = undefined

{- Helper functions -}

buildings = [ Building { buildingName = "The Institute, Building A"
                       , buildingDescription = "123 Blarg St."
                       , buildingId = 123 }
            , Building { buildingName = "The Institute, Building B"
                       , buildingDescription = "207 S. Foo St."
                       , buildingId = 456 }
            , Building { buildingName = "The Institute, Building C"
                       , buildingDescription = "127 Blarg St."
                       , buildingId = 789 } ]

rooms = [ Room { roomName = "Room 1"
               , roomDescription = "Dr. Evil"
               , roomBuildingId = 123
               , roomId = 66 }
        , Room { roomName = "Room 2"
               , roomDescription = "Mr. Tibbs"
               , roomBuildingId = 123
               , roomId = 77 } ]

