module Util where

import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Reader       (lift)
import Control.Monad.Trans.Either (EitherT, left)
import Data.List                  (groupBy, sortBy)
import Data.Map                   (Map, fromList)
import Data.Ord                   (comparing)
import Data.Text                  (Text)
import Data.Time.Clock            (getCurrentTime)
import Servant
import Servant.Server             (ServantErr)
import System.Environment         (lookupEnv)

import Types.Misc

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

groupPairs :: (Ord k) => [(k, v)] -> [(k, [v])]
groupPairs pairs = factorKeys . groupBy keysEqual . sortBy comparingKeys $ pairs
    where keysEqual x y = fst x == fst y
          comparingKeys = comparing fst
          factorKeys = map (\xs -> (fst . head $ xs, map snd xs))

groupList :: (Ord k) => (v -> k) -> [v] -> [(k, [v])]
groupList key vs = groupPairs . map toKeyValue $ vs
    where toKeyValue x = (key x, x)

groupMap :: (Ord k) => (v -> k) -> [v] -> Map k [v]
groupMap key vs = fromList . groupList key $ vs


