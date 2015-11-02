module Types.Model.Fields where

import Data.Aeson
import Data.Text
import Database.Persist.TH (derivePersistField)
import GHC.Generics (Generic)

type Email = Text

data PropertyType = FreeText
                  | Numeric
                  deriving (Eq, Generic, Ord, Read, Show)

instance ToJSON PropertyType where
  toJSON = genericToJSON defaultOptions
instance FromJSON PropertyType

derivePersistField "PropertyType"
