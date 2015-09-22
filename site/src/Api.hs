module Api where

import Servant.API
import Api.Types.Facilities
import Api.Types.Inventory
import Api.Types.Reports

{- TODO: Add authentication API -}

type InventoriumApi = FacilitiesApi
                 :<|> InventoryApi
                 :<|> ReportsApi
