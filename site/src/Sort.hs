module Sort where

import Control.Applicative ((<$>))
import Data.Maybe (isJust, fromJust)
import Servant
import Data.Text (append, intercalate, splitOn, uncons)

data SortOrder = Ascending | Descending
                 deriving (Eq, Ord, Show)

data SortField a = SortField SortOrder a
                   deriving (Eq, Ord, Show)


{- ToText and FromText instances for SortField -}

instance (ToText a) => ToText (SortField a) where
    toText (SortField Ascending x)  = toText x
    toText (SortField Descending x) = "-" `append` toText x

instance (FromText a) => FromText (SortField a) where
    fromText s = case uncons s of
                     Nothing        -> Nothing
                     Just ('-', s') -> SortField Descending <$> fromText s'
                     _              -> SortField Ascending <$> fromText s
