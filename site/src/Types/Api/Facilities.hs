module Types.Api.Facilities where

import Database.Persist (Key)
import Servant.API

import Types.Model.Building
import Types.Model.Room
import Types.Misc
import Types.Sort

type FacilitiesApi = 
    "api" :> "buildings"
        :> Authorized
        :> QueryParams "sort" (SortField BuildingSortBy)
        :> QueryParams "expand" BuildingExpand
        :> Get '[JSON] [BuildingDetail] :<|>
    "api" :> "buildings"
        :> Authorized
        :> ReqBody '[JSON] Building 
        :> Post '[JSON] BuildingDetail :<|>

    "api" :> "buildings" :> Capture "buildingId" (Key Building)
        :> Authorized
        :> QueryParams "expand" BuildingExpand
        :> Get '[JSON] BuildingDetail :<|>
    "api" :> "buildings" :> Capture "buildingId" (Key Building)
        :> Authorized
        :> ReqBody '[JSON] Building
        :> Put '[JSON] BuildingDetail :<|>
    "api" :> "buildings" :> Capture "buildingId" (Key Building)
        :> Authorized
        :> Delete '[JSON] () :<|>

    "api" :> "buildings" :> Capture "buildingId" (Key Building) :> "rooms"
        :> Authorized
        :> QueryParams "sort" (SortField RoomSortBy)
        :> QueryParams "expand" RoomExpand
        :> Get '[JSON] [RoomDetail] :<|>
    "api" :> "buildings" :> Capture "buildingId" (Key Building) :> "rooms"
        :> Authorized
        :> ReqBody '[JSON] Room 
        :> Post '[JSON] RoomDetail :<|>

    "api" :> "buildings" :> Capture "buildingId" (Key Building) :> "rooms" :> Capture "roomId" (Key Room)
        :> Authorized
        :> QueryParams "expand" RoomExpand
        :> Get '[JSON] RoomDetail  :<|>
    "api" :> "buildings" :> Capture "buildingId" (Key Building) :> "rooms" :> Capture "roomId" (Key Room)
        :> Authorized
        :> ReqBody '[JSON] Room
        :> Put '[JSON] RoomDetail  :<|>
    "api" :> "buildings" :> Capture "buildingId" (Key Building) :> "rooms" :> Capture "roomId" (Key Room)
        :> Authorized
        :> Delete '[JSON] ()
