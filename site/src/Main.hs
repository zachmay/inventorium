module Main where

import Control.Monad.Logger                 (runStdoutLoggingT)
import Control.Monad.Reader                 (ReaderT, runReaderT, lift)
import Control.Monad.Trans.Either           (EitherT, left)
import Data.ByteString.Char8                (pack)
import Data.List                            (intercalate)
import Database.Persist.Postgresql          (ConnectionPool, createPostgresqlPool, ConnectionString, runSqlPool)
import Network.Wai                          (Application)
import Network.Wai.Handler.Warp             (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant
import Servant.Docs

import Types.Api   (inventoriumApi)
import Types.App   (Config(..))
import Types.Misc  (Handler)
import Handlers    (allHandlers)
import Util        (lookupEnvironment, lookupSetting)
import Types.Model.Persistent (doMigrations)
import Docs

main :: IO ()
main = do
    port <- lookupSetting "PORT" 3000
    putStrLn "Starting Inventorium API server..."
    pool <- makePool
    let config = Config { getPool = pool }
    runSqlPool doMigrations pool
    putStrLn $ "Listening on port " ++ show port
    run port $ logStdout $ app config

app :: Config -> Application
app config = serve inventoriumApi $ enter (readerToEither config) allHandlers

readerToEither :: Config -> Handler :~> EitherT ServantErr IO
readerToEither config = Nat $ \x -> runReaderT x config

makePool :: IO ConnectionPool
makePool = do
    dbUser <- lookupEnvironment "DBUSER" "postgres"
    dbName <- lookupEnvironment "DBNAME" "postgres"
    dbPass <- lookupEnvironment "DBPASS" "postgres"
    dbHost <- lookupEnvironment "DBHOST" "database"
    dbPort <- lookupEnvironment "DBPORT" "5432"
    let connectionString = pack $ intercalate " " $ zipWith (++)
            ["host=", "dbname=", "user=", "password=", "port="] 
            [dbHost, dbName, dbUser, dbPass, dbPort] in
                runStdoutLoggingT $ createPostgresqlPool connectionString 4
