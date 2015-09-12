module Main where

import qualified Data.Set                 as Set
import qualified Data.UUID                as UUID
import qualified Data.UUID.V4             as UUID
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import Types.Api

server :: Server InventoriumApi
server = undefined

inventoriumApi :: Proxy InventoriumApi
inventoriumApi = Proxy

app :: Application
app = serve inventoriumApi server

main :: IO ()
main = do
    putStrLn "HELLO WORLD"
    run 8082 app
