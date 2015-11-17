module Types.Api where

import Servant

import Types.Api.Authentication (AuthenticationApi)
import Types.Api.Facilities     (FacilitiesApi)
import Types.Api.Inventory      (InventoryApi)

inventoriumApi :: Proxy InventoriumApi
inventoriumApi = Proxy

type InventoriumApi = AuthenticationApi
                 :<|> FacilitiesApi 
                 :<|> InventoryApi
