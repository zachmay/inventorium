module Handlers.Reports where

import Servant

import Handlers.Auth
import Types.Api.Reports
import Types.Misc
import Types.Model.ReconciliationReport
import Types.Model.ByTypeReport

reportsHandlers :: ServerT ReportsApi Handler
reportsHandlers = getReconciliationReport
             :<|> getByTypeReport

getReconciliationReport :: Maybe AuthToken -> Handler ReconciliationReport
getReconciliationReport auth = do
    checkAuthToken auth
    return $ ReconciliationReport {}

getByTypeReport :: Maybe AuthToken -> Handler ByTypeReport
getByTypeReport auth = do
    checkAuthToken auth
    return $ ByTypeReport {}
