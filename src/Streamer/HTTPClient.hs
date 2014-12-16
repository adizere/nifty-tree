module Streamer.HTTPClient where


import Streamer.Parents

import Network.HTTP
import Network.URI ( parseURI )
import Network.Stream (Result)
import qualified Data.ByteString.Lazy as L


doRequest :: String -> IO (Maybe L.ByteString)
doRequest url = do
    putStrLn $ "Executing request for URL: " ++ url
    rsp <- simpleHTTP $ getLazyRequest url
    isValid <- validateReponse rsp
    if isValid == True
        then getResponseBody rsp >>= (\rsBody -> return $ Just rsBody)
        else return Nothing


validateReponse :: Result (Response L.ByteString) -> IO Bool
validateReponse rsp = do
    responseCode <- getResponseCode rsp
    if responseCode == (2,0,0)
        then return True
        else do
            putStrLn $ "Error executing request. Response code: "
                       ++ (show responseCode)
            return False


getLazyRequest
    :: String                   -- ^URL to fetch
    -> Request L.ByteString  -- ^The constructed request
getLazyRequest urlString =
  case parseURI urlString of
    Nothing -> error ("getLazyRequest: Not a valid URL - " ++ urlString)
    Just u  -> mkRequest GET u



constructURL :: Parent -> Int -> String
constructURL node seqNr =
    "http://"
    ++ pnIp node ++ ":" ++ (show $ pnPort node)
    ++ "/" ++ (show seqNr) ++ ".frame"