module WebServer (WebServer.start) where

import Data.Aeson
import Network.HTTP.Server
import Network.Socket.Internal
import Network.URL
import Data.UnixTime
import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy as BL

import Storage

start :: (Storage s) => PortNumber -> s -> IO ()
start p s = serverWith defaultConfig{ srvPort = p } $ \_ url _ ->
  case url_path url of
    [] -> do
      xs <- Storage.all s
      return $ sendJSON xs
    k -> do
      xs <- Storage.top s k
      return $ sendJSON (k, xs)

instance ToJSON (Key, Top) where
  toJSON (k, (c,f,t,xs)) = object [
      "key" .= toJSON k
    , "from" .= toJSON (formatUnixTimeGMT "%Y-%m-%d %H:%M:%S" . fromEpochTime $ fromIntegral f)
    , "to" .= toJSON (formatUnixTimeGMT "%Y-%m-%d %H:%M:%S" . fromEpochTime $ fromIntegral t)
    , "count" .= toJSON c
    , "top" .= toJSON (Prelude.map (percent c) xs)
    ]

type Percent = Int

percent :: Count -> (Element,Count) -> (Element,Count,Percent)
percent t (e, c) = (e, c, round $ fromIntegral c / fromIntegral t * (100.0 :: Double))

instance ToJSON (Element,Count,Percent) where
  toJSON (e, c, p) = object [
      "element" .= e
    , "count" .= c
    , "percent" .= p
    ]

sendJSON :: (ToJSON a) => a -> Response String
sendJSON v =
  let body = BS.unpack . BS.concat . BL.toChunks $ encode v in
  insertHeader HdrContentType "application/json"
    . insertHeader (HdrCustom "Access-Control-Allow-Origin") "*"
    . insertHeader HdrContentLength (show (Prelude.length body))
    $ (respond OK :: Response String) {
        rspBody = body
      }