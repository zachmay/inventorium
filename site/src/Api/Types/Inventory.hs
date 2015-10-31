module Api.Types.Inventory where

import Servant.API
import Types
import Models
import Database.Persist (Entity, Key)
import Data.Text (Text)

type InventoryApi = 
    
    {- Inventory item types -}

    "api" :> "item-types"
        :> Authorized
        :> QueryParams "expand" ItemTypeExpand
        :> Get '[JSON] [ItemTypeDetail] :<|>
    "api" :> "item-types"
        :> Authorized
        :> ReqBody '[JSON] ItemType 
        :> Post '[JSON] (Entity ItemType) :<|>
    "api" :> "item-types" :> Capture "typeId" ItemTypeId
        :> Authorized
        :> Get '[JSON] (Entity ItemType) :<|>
    "api" :> "item-types" :> Capture "typeId" ItemTypeId
        :> Authorized
        :> ReqBody '[JSON] ItemType
        :> Put '[JSON] (Entity ItemType) :<|>
    "api" :> "item-types" :> Capture "typeId" ItemTypeId
        :> Authorized
        :> Delete '[JSON] () :<|>

    {- Inventory item collections -}

    "api" :> "inventory"
        :> Authorized
        :> QueryParam "sort" ItemSortBy
        :> QueryParams "expand" ItemExpand
        :> Get '[JSON] [Entity Item]  :<|>
    "api" :> "inventory"
        :> Authorized
        :> ReqBody '[JSON] Item
        :> Post '[JSON] (Entity Item) :<|>
    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "inventory"
        :> Authorized
        :> QueryParam "sort" ItemSortBy
        :> QueryParams "expand" ItemExpand
        :> Get '[JSON] [Entity Item] :<|>
    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "rooms" :> Capture "roomId" RoomId :> "inventory"
        :> Authorized
        :> QueryParam "sort" ItemSortBy
        :> QueryParams "expand" ItemExpand
        :> Get '[JSON] [Entity Item] :<|>
    "api" :> "buildings" :> Capture "buildingId" BuildingId :> "rooms" :> Capture "roomId" RoomId :> "inventory"
        :> Authorized
        :> ReqBody '[JSON] Item
        :> Post '[JSON] (Entity Item) :<|>

    {- Item resources -}

    "api" :> "inventory" :> Capture "itemId" ItemId
        :> Authorized
        :> QueryParams "expand" ItemExpand
        :> Get '[JSON] ItemDetail :<|> 
    "api" :> "inventory" :> Capture "itemId" ItemId
        :> Authorized
        :> ReqBody '[JSON] Item
        :> Put '[JSON] (Entity Item) :<|>
    "api" :> "inventory" :> Capture "itemId" ItemId
        :> Authorized
        :> Delete '[JSON] () :<|>

    {- Item check-in history collection -}

    "api" :> "inventory" :> Capture "itemId" ItemId :> "history"
        :> Authorized
        :> QueryParam "sort" CheckInSortBy
        :> QueryParams "expand" CheckInExpand
        :> Get '[JSON] [Entity CheckIn] :<|>
    "api" :> "inventory" :> Capture "itemId" ItemId :> "history"
        :> Authorized
        :> ReqBody '[JSON] CheckIn
        :> Post '[JSON] (Entity CheckIn) :<|>

    {- Item check-in resources -}

    "api" :> "inventory" :> Capture "itemId" ItemId :> "history" :> "latest"
        :> Authorized
        :> QueryParams "expand" CheckInExpand
        :> Get '[JSON] (Entity CheckIn) :<|>
    "api" :> "inventory" :> Capture "itemId" ItemId :> "history" :> Capture "checkinId" CheckInId
        :> Authorized
        :> QueryParams "expand" CheckInExpand
        :> Get '[JSON] (Entity CheckIn)
