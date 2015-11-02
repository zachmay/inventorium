module Types.Sort where

import Data.Text (append, uncons)
import Servant (ToText, toText, FromText, fromText)

{- Sorting Data Types -}

data SortOrder = Ascending | Descending
                 deriving (Eq, Ord, Show)

data SortField a = SortField SortOrder a
                   deriving (Eq, Ord, Show)

instance (ToText a) => ToText (SortField a) where
    toText (SortField Ascending x)  = toText x
    toText (SortField Descending x) = "-" `append` toText x

instance (FromText a) => FromText (SortField a) where
    fromText s = case uncons s of
                     Nothing        -> Nothing
                     Just ('-', s') -> SortField Descending <$> fromText s'
                     _              -> SortField Ascending <$> fromText s
