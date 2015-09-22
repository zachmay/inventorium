module Util where

import Control.Monad.Trans.Either (EitherT, left)
import Control.Monad.Reader (lift)
import Servant
import Types
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Servant.Server (ServantErr)
import Data.Time.Clock (getCurrentTime)
import System.Environment (lookupEnv)

consoleLog :: String -> Handler ()
consoleLog s = do
    t <- liftIO getCurrentTime
    liftIO . putStrLn $ show t ++ ": " ++ s

unimplemented :: String -> Handler a
unimplemented s = do
    consoleLog $ "Not implemented: " ++ s
    lift $ left $ ServantErr 501 "Not implemented" "This endpoint is not yet implemented" []

failWith :: ServantErr -> Handler a
failWith = lift . left

lookupEnvironment :: String -> String -> IO String
lookupEnvironment env def = do
    p <- lookupEnv env
    return $ case p of Nothing -> def
                       Just a  -> a

lookupSetting :: (Read a) => String -> a -> IO a
lookupSetting env def = do
    p <- lookupEnv env
    return $ case p of Nothing -> def
                       Just a  -> read a

