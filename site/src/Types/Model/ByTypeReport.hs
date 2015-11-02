module Types.Model.ByTypeReport (
    ByTypeReport(..)
) where

import Data.Aeson   (ToJSON, toJSON, Value(..))
import GHC.Generics (Generic)

data ByTypeReport = ByTypeReport {}
                  deriving (Eq, Show, Ord, Generic)

instance ToJSON ByTypeReport where
    toJSON r = Null
