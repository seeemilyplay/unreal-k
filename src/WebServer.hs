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

instance ToJSON (Key, Top) where
  toJSON (k, (c,xs)) = object [
      "key" .= toJSON k
    , "count" .= toJSON c
    , "top" .= toJSON xs
    ]

instance ToJSON (Element,Count) where
  toJSON (e, c) = object [
      "element" .= e
    , "count" .= c
    ]

sendJSON :: [(Key, Top)] -> Response String
sendJSON v =
  insertHeader HdrContentType "application/json"
    $ (respond OK :: Response String) {
        rspBody = BS.unpack . BS.concat . BL.toChunks $ encode v
      }