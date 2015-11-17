module Types.Api.Inventory where

import Database.Persist (Key)
import Servant.API

import Types.Model.Building
import Types.Model.CheckIn
import Types.Model.Room
import Types.Model.Item
import Types.Model.ItemType
import Types.Misc
import Types.Sort (SortField)

type InventoryApi = 
    
    {- Inventory item types -}

    "api" :> "item-types"
        :> Authorized
        :> QueryParams "expand" ItemTypeExpand
        :> Get '[JSON] [ItemTypeDetail] :<|>
    "api" :> "item-types"
        :> Authorized
        :> ReqBody '[JSON] ItemType 
        :> Post '[JSON] ItemTypeDetail :<|>
    "api" :> "item-types" :> Capture "typeId" ItemTypeId
        :> Authorized
        :> Get '[JSON] ItemTypeDetail :<|>
    "api" :> "item-types" :> Capture "typeId" ItemTypeId
        :> Authorized
        :> ReqBody '[JSON] ItemType
        :> Put '[JSON] ItemTypeDetail :<|>
    "api" :> "item-types" :> Capture "typeId" ItemTypeId
        :> Authorized
        :> Delete '[JSON] () :<|>

    {- Inventory item collections -}

    "api" :> "inventory"
        :> Authorized
        :> QueryParams "sort" (SortField ItemSortBy)
        :> QueryParams "expand" ItemExpand
        :> Get '[JSON] [ItemDetail]  :<|>
    "api" :> "inventory"
        :> Authorized
        :> ReqBody '[JSON] Item
        :> Post '[JSON] ItemDetail :<|>
    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "inventory"
        :> Authorized
        :> QueryParams "sort" (SortField ItemSortBy)
        :> QueryParams "expand" ItemExpand
        :> Get '[JSON] [ItemDetail] :<|>
    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "rooms" :> Capture "roomId" RoomId :> "inventory"
        :> Authorized
        :> QueryParams "sort" (SortField ItemSortBy)
        :> QueryParams "expand" ItemExpand
        :> Get '[JSON] [ItemDetail] :<|>
    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "rooms" :> Capture "roomId" RoomId :> "inventory"
        :> Authorized
        :> ReqBody '[JSON] Item
        :> Post '[JSON] ItemDetail :<|>

    {- Item resources -}

    "api" :> "inventory" :> Capture "itemId" ItemId
        :> Authorized
        :> QueryParams "expand" ItemExpand
        :> Get '[JSON] ItemDetail :<|> 
    "api" :> "inventory" :> Capture "itemId" ItemId
        :> Authorized
        :> ReqBody '[JSON] Item
        :> Put '[JSON] ItemDetail :<|>
    "api" :> "inventory" :> Capture "itemId" ItemId
        :> Authorized
        :> Delete '[JSON] () :<|>

    {- Item check-in history collection -}

    "api" :> "inventory" :> Capture "itemId" ItemId :> "history"
        :> Authorized
        :> QueryParams "sort" (SortField CheckInSortBy)
        :> QueryParams "expand" CheckInExpand
        :> Get '[JSON] [CheckInDetail] :<|>
    "api" :> "inventory" :> Capture "itemId" ItemId :> "history"
        :> Authorized
        :> ReqBody '[JSON] CheckIn
        :> Post '[JSON] CheckInDetail :<|>

    {- Item check-in resources -}

    "api" :> "inventory" :> Capture "itemId" ItemId :> "history" :> "latest"
        :> Authorized
        :> QueryParams "expand" CheckInExpand
        :> Get '[JSON] CheckInDetail :<|>
    "api" :> "inventory" :> Capture "itemId" ItemId :> "history" :> Capture "checkinId" CheckInId
        :> Authorized
        :> QueryParams "expand" CheckInExpand
        :> Get '[JSON] CheckInDetail
