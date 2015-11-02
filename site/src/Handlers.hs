module Handlers where

import Servant

import Handlers.Authentication
import Handlers.Facilities
import Handlers.Inventory
import Handlers.Reports

allHandlers = authenticationHandlers
         :<|> facilitiesHandlers
         :<|> inventoryHandlers
         :<|> reportsHandlers
