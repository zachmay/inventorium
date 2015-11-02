module Types.App where

import Database.Persist.Postgresql (ConnectionPool)

data Config = Config
    { getPool :: ConnectionPool }
