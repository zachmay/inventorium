module Main where

import Network.Wai.Middleware.RequestLogger (logStdout)
import qualified Data.Set                 as Set
import qualified Data.UUID                as UUID
import qualified Data.UUID.V4             as UUID
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Api.Types.Facilities
import qualified Api.Handlers.Facilities as F
import           Api.Types.Inventory
import qualified Api.Handlers.Inventory as I          
import           Util
import Models (doMigrations)
import Database.Persist.Postgresql (ConnectionPool, createPostgresqlPool, ConnectionString, runSqlPool)
import Control.Monad.Logger                 (runNoLoggingT, runStdoutLoggingT)
import Data.List (intercalate)
import Data.ByteString.Char8 (pack)
import Config
import Control.Monad.Reader         (ReaderT, runReaderT, lift)
import Control.Monad.Trans.Either   (EitherT, left)


type FullApi = FacilitiesApi :<|> InventoryApi

fullServer :: ServerT FullApi AppM
fullServer = facServer :<|> invServer

facServer :: ServerT FacilitiesApi AppM
facServer = F.getBuildingList
       :<|> F.postBuildingList
       :<|> F.getBuilding
       :<|> F.putBuilding
       :<|> F.patchBuilding
       :<|> F.deleteBuilding
       :<|> F.getRoomList
       :<|> F.postRoomList
       :<|> F.getRoom
       :<|> F.putRoom
       :<|> F.patchRoom
       :<|> F.deleteRoom

invServer :: ServerT InventoryApi AppM
invServer = I.getMasterInventory
       :<|> I.postMasterInventory
       :<|> I.getBuildingInventory
       :<|> I.getRoomInventory
       :<|> I.postRoomInventory
       :<|> I.getItem
       :<|> I.putItem
       :<|> I.patchItem
       :<|> I.deleteItem
       :<|> I.getItemHistory
       :<|> I.postItemHistory
       :<|> I.getItemLatestCheckIn
       :<|> I.getItemCheckIn

type AppM = ReaderT Config (EitherT ServantErr IO)

api :: Proxy FullApi
api = Proxy

app :: Config -> Application
app config = serve api $ enter (readerToEither config) fullServer

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither config = Nat $ \x -> runReaderT x config

makePool :: IO ConnectionPool
makePool = do
    dbUser <- lookupEnvironment "DBUSER" "postgres"
    dbName <- lookupEnvironment "DBNAME" "postgres"
    dbPass <- lookupEnvironment "DBPASS" "postgres"
    dbHost <- lookupEnvironment "DBHOST" "database"
    dbPort <- lookupEnvironment "DBPORT" "5432"
    let connectionString = pack $ intercalate " " $ zipWith (++) ["host=", "dbname=", "user=", "password=", "port="] 
                                                                 [dbHost, dbName, dbUser, dbPass, dbPort] in
        runStdoutLoggingT $ createPostgresqlPool connectionString 4

main :: IO ()
main = do
    port <- lookupSetting "PORT" 3000
    putStrLn "Starting Inventorium API server..."
    pool <- makePool
    let config = Config { getPool = pool }
    runSqlPool doMigrations pool
    putStrLn $ "Listening on port " ++ show port
    run port $ logStdout $ app config

