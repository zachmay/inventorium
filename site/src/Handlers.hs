module Handlers where

import Servant

import Handlers.Authentication
import Handlers.Facilities
import Handlers.Inventory

allHandlers = authenticationHandlers
         :<|> facilitiesHandlers
         :<|> inventoryHandlers
