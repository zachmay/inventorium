module Queries.Facilities where

import Database.Esqueleto

import Models
import Types

{-
getItemCurrentCheckIn :: ItemId -> Handler [(Entity Item, Entity CheckIn, Entity Room)]
getItemCurrentCheckIn itemId = runDb $ select $ from $ \(i, c, r) -> do
    where_ (i ^. ItemId ==. val itemId)
    where_ (i ^. ItemCurrentCheckIn ==. c ^. CheckInId)
    where_ (i ^. CheckInRoom ==. r ^. RoomId)
    return (i, c, r)
-}

getItemCurrentCheckIn itemId = 
    select $
    from $ \(i `LeftOuterJoin` c) -> do
    on (i ^. ItemCurrentCheckIn ==. c ?. CheckInId)
    where_ (i ^. ItemId ==. val itemId)
    return (i, c)
