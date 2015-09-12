module Types.Api.Facilities (FacilitiesApi) where

import Servant.API
import Types

type FacilitiesApi = BuildingListApi
                :<|> BuildingApi
                :<|> RoomListApi
                :<|> RoomApi

type BuildingListApi =
    "api" :> "buildings"
        :> Authorized
        :> QueryParam "sort" BuildingSortBy
        :> QueryParams "expand" BuildingExpand
        :> Get '[JSON] [Building] :<|>
    "api" :> "buildings"
        :> Authorized
        :> ReqBody '[JSON] Building 
        :> Post '[JSON] Building

type BuildingApi =
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
        :> Delete '[JSON] ()

type RoomListApi =
    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "rooms"
        :> Authorized
        :> QueryParams "sort" RoomSortBy
        :> QueryParams "expand" RoomExpand
        :> Get '[JSON] [Room] :<|>
    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "rooms"
        :> Authorized
        :> ReqBody '[JSON] Room 
        :> Post '[JSON] Room

type RoomApi =
    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "rooms" :> Capture "roomId" RoomId
        :> Authorized
        :> QueryParams "expand" RoomExpand
        :> Get '[JSON] Room :<|>
    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "rooms" :> Capture "roomId" RoomId
        :> Authorized
        :> ReqBody '[JSON] Room
        :> Put '[JSON] Room :<|>
    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "rooms" :> Capture "roomId" RoomId
        :> Authorized
        :> ReqBody '[JSON] Room
        :> Patch '[JSON] Room :<|>
    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "rooms" :> Capture "roomId" RoomId
        :> Authorized
        :> Delete '[JSON] Room
