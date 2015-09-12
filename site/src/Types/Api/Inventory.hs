module Types.Api.Inventory where

import Servant.API
import Types

type InventoryApi = InventoryListApi
               :<|> InventoryItemApi 
               :<|> ItemHistoryApi

type InventoryListApi =
    "api" :> "inventory"
        :> Authorized
        :> QueryParam "sort" ItemSortBy
        :> QueryParams "expand" ItemExpand
        :> Get '[JSON] [Item] :<|>
    "api" :> "inventory"
        :> Authorized
        :> ReqBody '[JSON] Item
        :> Post '[JSON] Item :<|>
    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "rooms" :> Capture "roomId" RoomId
        :> Authorized
        :> QueryParam "sort" ItemSortBy
        :> QueryParams "expand" ItemExpand
        :> Get '[JSON] [Item] :<|>
    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "rooms" :> Capture "roomId" RoomId
        :> Authorized
        :> ReqBody '[JSON] Item
        :> Post '[JSON] Item
    

type InventoryItemApi =
    "api" :> "inventory" :> Capture "itemId" ItemId
        :> Authorized
        :> QueryParam "expand" ItemExpand
        :> Get '[JSON] Item :<|>
    "api" :> "inventory" :> Capture "itemId" ItemId
        :> Authorized
        :> ReqBody '[JSON] Item
        :> Put '[JSON] Item :<|>
    "api" :> "inventory" :> Capture "itemId" ItemId
        :> Authorized
        :> ReqBody '[JSON] Item
        :> Patch '[JSON] Item :<|>
    "api" :> "inventory" :> Capture "itemId" ItemId
        :> Authorized
        :> Delete '[JSON] ()

type ItemHistoryApi =
    "api" :> "inventory" :> Capture "itemId" ItemId :> "history"
        :> Authorized
        :> QueryParams "expand" CheckInExpand
        :> Get '[JSON] [CheckIn] :<|>
    "api" :> "inventory" :> Capture "itemId" ItemId :> "history"
        :> Authorized
        :> ReqBody '[JSON] CheckIn
        :> Post '[JSON] CheckIn :<|>
    "api" :> "inventory" :> Capture "itemId" ItemId :> "history" :> "latest"
        :> Authorized
        :> QueryParams "expand" CheckInExpand
        :> Get '[JSON] CheckIn :<|>
    "api" :> "inventory" :> Capture "itemId" ItemId :> "history" :> Capture "checkinId" CheckInId
        :> Authorized
        :> QueryParams "expand" CheckInExpand
        :> Get '[JSON] CheckIn
