module Types.Model.ReconciliationReport (
    ReconciliationReport(..)
) where

import Data.Aeson   (ToJSON, toJSON, Value(..))
import GHC.Generics (Generic)

data ReconciliationReport = ReconciliationReport {}
                            deriving (Generic)

instance ToJSON ReconciliationReport where
    toJSON r = Null
