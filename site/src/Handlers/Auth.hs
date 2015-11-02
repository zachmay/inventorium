module Handlers.Auth where

import Types.Misc
import Control.Monad.IO.Class (liftIO);
import Control.Monad.Trans.Either
import Servant.Server
import Util

checkAuthToken :: Maybe AuthToken -> Handler ()
checkAuthToken Nothing = do
    consoleLog "Failed auth: nothing supplied" 
    failWith $ ServantErr 401 "Unauthorized" "Invalid authorization token" []
checkAuthToken (Just _) = do
    {- TODO: Validate auth token -}
    return ()
