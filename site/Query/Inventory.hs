module Query.Inventory where

import Import

getInventoryList = do
    runDB $ selectList [] [Asc BuildingId]
