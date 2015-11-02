module Handlers.Authentication where

import Servant.API

import Types.Misc

authenticationHandlers = postAuth

postAuth :: AuthRequest -> Handler AuthResponse
postAuth (AuthRequest { authUserName = u, authPassword = p }) = do
    if (u == "user" && p == "password")
        then return $ AuthSuccess "abc123"
        else return $ AuthFailure "Invalid username or password."
