module Queries.Inventory where

import Database.Esqueleto

import Models
import Types

getItemTypesWithProperties :: Query [(Entity ItemType, Entity ItemTypeProperty)]
getItemTypesWithProperties =
    select $
    from $ \(it, p) -> do
    where_ (it ^. ItemTypeId ==. p ^. ItemTypePropertyItemType)
    return (it, p)

getItemWithCurrentCheckIn :: ItemId -> Query [(Entity Item, Maybe (Entity CheckIn))]
getItemWithCurrentCheckIn itemId = 
    select $
    from $ \(i `LeftOuterJoin` c) -> do
    on (i ^. ItemCurrentCheckIn ==. c ?. CheckInId)
    where_ (i ^. ItemId ==. val itemId)
    return (i, c)
