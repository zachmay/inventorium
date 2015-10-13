module Api.Handlers.Reports where

import Servant.API
import Types

type ReportsApi =
    "api" :> "reports" :> "reconciliation"
        :> Authorized
        :> Get '[JSON] ReconciliationReport :<|>
    "api" :> "reports" :> "by-type"
        :> Authorized
        :> Get '[JSON] ByTypeReport
    
