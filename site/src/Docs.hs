module Docs where

import Database.Persist     (Entity(..), Key)
import Database.Persist.Sql (toSqlKey)
import Data.Time.Clock      (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar 
import Servant
import Servant.Docs

import Types.Api
import Types.Api.Facilities
import Types.Misc
import Types.Model.Building
import Types.Model.CheckIn
import Types.Model.Item
import Types.Model.ItemType
import Types.Model.Persistent
import Types.Model.Room
import Types.Sort (SortField)

{- Check-ins -}

instance ToSample [CheckInDetail] [CheckInDetail] where
    toSamples _ = [ ("Blah", [sampleCheckInDetail]), ("blah blah", [sampleCheckInDetail]) ]

instance ToSample CheckInDetail CheckInDetail where
    toSample _ = Just sampleCheckInDetail

instance ToSample CheckIn CheckIn where
    toSample _ = Just sampleCheckIn

sampleCheckInDetail =
    CheckInDetail { checkIn = entity
                  , room = Nothing
                  , user = Nothing }
                      where entity = Entity (toSqlKey 99) sampleCheckIn

sampleCheckIn = CheckIn (toSqlKey 2)
                        (toSqlKey 3)
                        sampleDate
                        (toSqlKey 4)

instance ToCapture (Capture "checkinId" (Key CheckIn)) where
    toCapture _ = DocCapture "checkinId" "The ID of the check-in."

instance ToParam (QueryParams "expand" CheckInExpand) where
    toParam _ = DocQueryParam { _paramName   = "expand"
                              , _paramValues = ["room", "user"]
                              , _paramDesc   = "Optional related records to retrieve."
                              , _paramKind   = List }

instance ToParam (QueryParams "sort" (SortField CheckInSortBy)) where
    toParam _ = DocQueryParam { _paramName   = "sort"
                              , _paramValues = ["[-]date"]
                              , _paramDesc   = "Optional sort key, use `-` for descending sort."
                              , _paramKind   = List }

{- Inventory items -}

instance ToSample [ItemDetail] [ItemDetail] where
    toSample _ = Nothing

instance ToSample ItemDetail ItemDetail where
    toSample _ = Nothing

instance ToSample Item Item where
    toSample _ = Nothing

instance ToCapture (Capture "itemId" (Key Item)) where
    toCapture _ = DocCapture "itemId" "The ID of the item."

instance ToParam (QueryParams "expand" ItemExpand) where
    toParam _ = DocQueryParam { _paramName   = "expand"
                              , _paramValues = ["checkins"]
                              , _paramDesc   = "Optional related records to retrieve."
                              , _paramKind   = List }

instance ToParam (QueryParams "sort" (SortField ItemSortBy)) where
    toParam _ = DocQueryParam { _paramName   = "sort"
                              , _paramValues = ["[-]created", "[-]updated", "[-]checkin", "[-]name"]
                              , _paramDesc   = "Optional sort key, use `-` for descending sort."
                              , _paramKind   = List }

{- Inventory item types -}

instance ToSample [ItemTypeDetail] [ItemTypeDetail] where
    toSample _ = Nothing

instance ToSample ItemTypeDetail ItemTypeDetail where
    toSample _ = Nothing

instance ToSample ItemType ItemType where
    toSample _ = Nothing

instance ToCapture (Capture "typeId" (Key ItemType)) where
    toCapture _ = DocCapture "typeId" "The ID of the item type."

instance ToParam (QueryParams "expand" ItemTypeExpand) where
    toParam _ = DocQueryParam { _paramName   = "expand"
                              , _paramValues = ["properties"]
                              , _paramDesc   = "Optional related records to retrieve."
                              , _paramKind   = List }

{- Rooms -} 

instance ToSample [RoomDetail] [RoomDetail] where
    toSample _ = Nothing

instance ToSample RoomDetail RoomDetail where
    toSample _ = Nothing

instance ToSample Room Room where
    toSample _ = Nothing

instance ToCapture (Capture "roomId" (Key Room)) where
    toCapture _ = DocCapture "roomId" "The ID of the room."

instance ToParam (QueryParams "expand" RoomExpand) where
    toParam _ = DocQueryParam { _paramName   = "expand"
                              , _paramValues = ["inventory"]
                              , _paramDesc   = "Optional related records to retrieve."
                              , _paramKind   = List }

instance ToParam (QueryParams "sort" (SortField RoomSortBy)) where
    toParam _ = DocQueryParam { _paramName   = "sort"
                              , _paramValues = ["[-]created", "[-]updated", "[-]name", "[-]description"]
                              , _paramDesc   = "Optional sort key, use `-` for descending sort."
                              , _paramKind   = List }


{- Buildings -}

instance ToSample [BuildingDetail] [BuildingDetail] where
    toSample _ = Nothing

instance ToSample BuildingDetail BuildingDetail where
    toSample _ = Nothing

instance ToSample Building Building where
    toSample _ = Nothing

instance ToCapture (Capture "buildingId" (Key Building)) where
    toCapture _ = DocCapture "buildingId" "The ID of the building."

instance ToParam (QueryParams "expand" BuildingExpand) where
    toParam _ = DocQueryParam { _paramName   = "expand"
                              , _paramValues = ["rooms"]
                              , _paramDesc   = "Optional related records to retrieve."
                              , _paramKind   = List }

instance ToParam (QueryParams "sort" (SortField BuildingSortBy)) where
    toParam _ = DocQueryParam { _paramName   = "sort"
                              , _paramValues = ["[-]created", "[-]updated", "[-]description", "[-]name"]
                              , _paramDesc   = "Optional sort key, use `-` for descending sort"
                              , _paramKind   = List }

{- Miscellaneous -}

instance ToSample () () where
    toSample _ = Just ()

instance ToSample AuthRequest AuthRequest where
    toSample _ = Nothing

instance ToSample AuthResponse AuthResponse where
    toSample _ = Nothing

sampleDate = UTCTime { utctDay = fromGregorian 2015 11 4
                     , utctDayTime = secondsToDiffTime $ 60 * 60 * 11 }

apiDocs :: API
apiDocs = docs inventoriumApi
