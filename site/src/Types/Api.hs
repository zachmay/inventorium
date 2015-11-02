module Types.Api where

import Servant

import Types.Api.Authentication (AuthenticationApi)
import Types.Api.Facilities     (FacilitiesApi)
import Types.Api.Inventory      (InventoryApi)
import Types.Api.Reports        (ReportsApi)

type InventoriumApi = AuthenticationApi
                 :<|> FacilitiesApi 
                 :<|> InventoryApi
                 :<|> ReportsApi
