module Api.Types.Facilities where

import Servant.API
import Types
import Data.Text (Text)

type FacilitiesApi = 
    "api" :> "buildings"
        :> Authorized
        :> QueryParam "sort" BuildingSortBy
        :> QueryParams "expand" BuildingExpand
        :> Get '[JSON] [Building] :<|>
    "api" :> "buildings"
        :> Authorized
        :> ReqBody '[JSON] Building 
        :> Post '[JSON] Building :<|>

    "api" :> "buildings" :> Capture "buildingId" BuildingId
        :> Authorized
        :> QueryParams "expand" BuildingExpand
        :> Get '[JSON] Building :<|>
    "api" :> "buildings" :> Capture "buildingId" BuildingId
        :> Authorized
        :> ReqBody '[JSON] Building
        :> Put '[JSON] Building :<|>
    "api" :> "buildings" :> Capture "buildingId" BuildingId
        :> Authorized
        :> ReqBody '[JSON] Building
        :> Patch '[JSON] Building :<|>
    "api" :> "buildings" :> Capture "buildingId" BuildingId
        :> Authorized
        :> Delete '[JSON] () :<|>

    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "rooms"
        :> Authorized
        :> QueryParam "sort" RoomSortBy
        :> QueryParams "expand" RoomExpand
        :> Get '[JSON] [Room] :<|>
    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "rooms"
        :> Authorized
        :> ReqBody '[JSON] Room 
        :> Post '[JSON] Room :<|>

    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "rooms" :> Capture "roomId" RoomId
        :> Authorized
        :> QueryParams "expand" RoomExpand
        :> Get '[JSON] Room  :<|>
    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "rooms" :> Capture "roomId" RoomId
        :> Authorized
        :> ReqBody '[JSON] Room
        :> Put '[JSON] Room  :<|>
    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "rooms" :> Capture "roomId" RoomId
        :> Authorized
        :> ReqBody '[JSON] Room
        :> Patch '[JSON] Room :<|>
    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "rooms" :> Capture "roomId" RoomId
        :> Authorized
        :> Delete '[JSON] ()
