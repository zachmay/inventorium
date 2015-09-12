module Types.Api where

import Servant.API
import Types.Api.Facilities
import Types.Api.Inventory
import Types.Api.Reports

type InventoriumApi = FacilitiesApi
                 :<|> InventoryApi
                 :<|> ReportsApi
