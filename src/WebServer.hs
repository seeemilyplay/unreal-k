module WebServer (WebServer.start) where

import Data.Aeson
import Network.HTTP.Server
import Network.Socket.Internal
import Network.URL
import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy as BL

import AcidServer

start :: PortNumber -> AcidServer -> IO ()
start p acid = serverWith defaultConfig{ srvPort = p } $ \_ url _ ->
  case url_path url of
    [] -> do
      xs <- AcidServer.all acid
      return $ sendJSON xs
    k -> do
      xs <- AcidServer.top acid k
      return $ sendJSON [(k, xs)]

instance ToJSON (Key, [Item]) where
  toJSON (k, xs) = object [
      "key" .= toJSON k
    , "top" .= toJSON xs
    ]

instance ToJSON Item where
  toJSON (e, c, t) = object [
      "element" .= e
    , "count" .= c
    , "time" .= t
    ]

sendJSON :: [(Key, [Item])] -> Response String
sendJSON v =
  insertHeader HdrContentType "application/json"
    $ (respond OK :: Response String) {
        rspBody = BS.unpack . BS.concat . BL.toChunks $ encode v
      }