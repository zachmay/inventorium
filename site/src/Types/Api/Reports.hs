module Types.Api.Reports where

import Servant.API

import Types.Misc
import Types.Model.ByTypeReport
import Types.Model.ReconciliationReport

type ReportsApi =
    "api" :> "reports" :> "reconciliation"
        :> Authorized
        :> Get '[JSON] ReconciliationReport :<|>
    "api" :> "reports" :> "by-type"
        :> Authorized
        :> Get '[JSON] ByTypeReport
    
