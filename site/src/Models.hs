module Models where

import Database.Persist.TH (share, mkPersist, sqlSettings, mkMigrate, persistFileWith)
import Database.Persist.Quasi (lowerCaseSettings)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Typeable
import Data.Aeson                  (ToJSON, FromJSON)
import GHC.Generics                (Generic)
import Control.Monad.Reader        (ReaderT, asks, liftIO)
import Database.Persist.Postgresql (SqlBackend(..), runMigration, runSqlPool)
import Config

type Email = Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "src/model")

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

