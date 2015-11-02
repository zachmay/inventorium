module Types.Model.Persistent where

import Control.Monad.Reader        (ReaderT, asks, liftIO)
import Data.Text                   (Text)
import Data.Time.Clock             (UTCTime)
import Data.Typeable               (Typeable)
import Database.Persist.Postgresql (SqlBackend(..), runMigration, runSqlPool)
import Database.Persist.Quasi      (lowerCaseSettings)
import Database.Persist.Sql        (SqlPersistT)
import Database.Persist.TH         (share, mkPersist, sqlSettings, mkMigrate, persistFileWith)

import Types.App
import Types.Misc
import Types.Model.Fields

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "src/model")

{-
 - Database-related helper functions -} 

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

runDb :: SqlPersistT IO b -> Handler b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

