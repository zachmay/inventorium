module Api.Types.Inventory where

import Servant.API
import Types

type InventoryApi = 
    
    {- Inventory item collections -}

    "api" :> "inventory"
        :> Authorized
        :> QueryParam "sort" ItemSortBy
        :> QueryParams "expand" ItemExpand
        :> Get '[JSON] [Item]  :<|>
    "api" :> "inventory"
        :> Authorized
        :> ReqBody '[JSON] Item
        :> Post '[JSON] Item :<|>
    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "inventory"
        :> Authorized
        :> QueryParam "sort" ItemSortBy
        :> QueryParams "expand" ItemExpand
        :> Get '[JSON] [Item] :<|>
    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "rooms" :> Capture "roomId" RoomId :> "inventory"
        :> Authorized
        :> QueryParam "sort" ItemSortBy
        :> QueryParams "expand" ItemExpand
        :> Get '[JSON] [Item] :<|>
    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "rooms" :> Capture "roomId" RoomId :> "inventory"
        :> Authorized
        :> ReqBody '[JSON] Item
        :> Post '[JSON] Item :<|>

    {- Item resources -}

    "api" :> "inventory" :> Capture "itemId" ItemId
        :> Authorized
        :> QueryParams "expand" ItemExpand
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
        :> Delete '[JSON] () :<|>

    {- Item check-in history collection -}

    "api" :> "inventory" :> Capture "itemId" ItemId :> "history"
        :> Authorized
        :> QueryParam "sort" CheckInSortBy
        :> QueryParams "expand" CheckInExpand
        :> Get '[JSON] [CheckIn] :<|>
    "api" :> "inventory" :> Capture "itemId" ItemId :> "history"
        :> Authorized
        :> ReqBody '[JSON] CheckIn
        :> Post '[JSON] CheckIn :<|>

    {- Item check-in resources -}

    "api" :> "inventory" :> Capture "itemId" ItemId :> "history" :> "latest"
        :> Authorized
        :> QueryParams "expand" CheckInExpand
        :> Get '[JSON] CheckIn :<|>
    "api" :> "inventory" :> Capture "itemId" ItemId :> "history" :> Capture "checkinId" CheckInId
        :> Authorized
        :> QueryParams "expand" CheckInExpand
        :> Get '[JSON] CheckIn
