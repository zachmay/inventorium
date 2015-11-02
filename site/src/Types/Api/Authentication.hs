module Types.Api.Authentication where

import Database.Persist (Entity, Key)
import Servant.API

import Types.Misc

type AuthenticationApi = 
    "api" :> "auth"
        :> ReqBody '[JSON] AuthRequest 
        :> Post '[JSON] AuthResponse


