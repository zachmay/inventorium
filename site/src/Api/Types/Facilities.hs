module Api.Types.Facilities where

import Servant.API
import Types
import Models
import Database.Persist (Entity, Key)
import Data.Text (Text)

type FacilitiesApi = 
    "api" :> "buildings"
        :> Authorized
        :> QueryParam "sort" BuildingSortBy
        :> QueryParams "expand" BuildingExpand
        :> Get '[JSON] [Entity Building] :<|>
    "api" :> "buildings"
        :> Authorized
        :> ReqBody '[JSON] Building 
        :> Post '[JSON] (Entity Building) :<|>

    "api" :> "buildings" :> Capture "buildingId" (Key Building)
        :> Authorized
        :> QueryParams "expand" BuildingExpand
        :> Get '[JSON] (Entity Building) :<|>
    "api" :> "buildings" :> Capture "buildingId" (Key Building)
        :> Authorized
        :> ReqBody '[JSON] Building
        :> Put '[JSON] (Entity Building) :<|>
    "api" :> "buildings" :> Capture "buildingId" (Key Building)
        :> Authorized
        :> Delete '[JSON] () :<|>

    "api" :> "buildings" :> Capture "buildingId" (Key Building) :> "rooms"
        :> Authorized
        :> QueryParam "sort" RoomSortBy
        :> QueryParams "expand" RoomExpand
        :> Get '[JSON] [Entity Room] :<|>
    "api" :> "buildings" :> Capture "buildingId" (Key Building) :> "rooms"
        :> Authorized
        :> ReqBody '[JSON] Room 
        :> Post '[JSON] (Entity Room) :<|>

    "api" :> "buildings" :> Capture "buildingId" (Key Building) :> "rooms" :> Capture "roomId" (Key Room)
        :> Authorized
        :> QueryParams "expand" RoomExpand
        :> Get '[JSON] (Entity Room)  :<|>
    "api" :> "buildings" :> Capture "buildingId" (Key Building) :> "rooms" :> Capture "roomId" (Key Room)
        :> Authorized
        :> ReqBody '[JSON] Room
        :> Put '[JSON] (Entity Room)  :<|>
    "api" :> "buildings" :> Capture "buildingId" (Key Building) :> "rooms" :> Capture "roomId" (Key Room)
        :> Authorized
        :> Delete '[JSON] ()
