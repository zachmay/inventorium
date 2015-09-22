module Config where

import Database.Persist.Postgresql          (ConnectionPool, createPostgresqlPool, ConnectionString)
import Control.Monad.Logger                 (runNoLoggingT, runStdoutLoggingT)

data Config = Config
    { getPool :: ConnectionPool }



