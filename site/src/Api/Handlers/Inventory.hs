module Api.Handlers.Inventory where

import Servant.API
import Types
import Api.Types.Inventory
import Control.Monad.IO.Class (liftIO)

die :: String -> Handler a
die s = do
    liftIO $ putStrLn s
    undefined

getMasterInventory :: Maybe AuthToken -> Maybe ItemSortBy -> [ItemExpand] -> Handler [Item]
getMasterInventory auth sort expand = die "getMasterInventory"

postMasterInventory :: Maybe AuthToken -> Item -> Handler Item
postMasterInventory auth item = die "postMasterInventory"

getBuildingInventory :: BuildingId -> Maybe AuthToken -> Maybe ItemSortBy -> [ItemExpand] -> Handler [Item]
getBuildingInventory bid auth sort expand = die "getBuildingInventory"

getRoomInventory :: BuildingId -> RoomId -> Maybe AuthToken -> Maybe ItemSortBy -> [ItemExpand] -> Handler [Item]
getRoomInventory bid rid auth sort expand = die "getRoomInventory"

postRoomInventory :: BuildingId -> RoomId -> Maybe AuthToken -> Item -> Handler Item
postRoomInventory bid rid auth item = die "postRoomInventory"

{- Individual item resources -}

getItem :: ItemId -> Maybe AuthToken -> [ItemExpand] -> Handler Item
getItem iid auth expand = die "getItem"

putItem :: ItemId -> Maybe AuthToken -> Item -> Handler Item
putItem iid auth item = die "putItem"

patchItem :: ItemId -> Maybe AuthToken -> Item -> Handler Item
patchItem iid auth item = die "patchItem"

deleteItem :: ItemId -> Maybe AuthToken -> Handler ()
deleteItem iid auth = die "deleteItem"

{- Item check-in history collection -}

getItemHistory :: ItemId -> Maybe AuthToken -> Maybe CheckInSortBy -> [CheckInExpand] -> Handler [CheckIn]
getItemHistory iid auth sort expand = die "getItemHistory"

postItemHistory :: ItemId -> Maybe AuthToken -> CheckIn -> Handler CheckIn
postItemHistory iid auth checkin = die "postItemHistory"

{- Item check-in resources -}

getItemLatestCheckIn :: ItemId -> Maybe AuthToken -> [CheckInExpand] -> Handler CheckIn
getItemLatestCheckIn iid auth expand = die "getItemLatestCheckIn"

getItemCheckIn :: ItemId -> CheckInId -> Maybe AuthToken -> [CheckInExpand] -> Handler CheckIn
getItemCheckIn iid cid auth expand = die "getItemCheckIn"
