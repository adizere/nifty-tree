module Streamer.HTTPClient where


import Streamer.Util                    (maybeRead)

import Network.URI                      (parseURI)
import Network.Stream                   (Result)
import Network.HTTP.Headers             ( replaceHeader
                                        , HeaderName(HdrIfNoneMatch, HdrETag)
                                        , findHeader )
import qualified Network.HTTP.Base      as HB
import qualified Network.HTTP           as H
import qualified Data.ByteString.Lazy   as L
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as C


httpGetFrameBytes :: String -> IO (Maybe L.ByteString)
httpGetFrameBytes url = do
    -- putStrLn $ "Executing request for URL: " ++ url
    rsp <- H.simpleHTTP $ simpleLazyRequest url
    isValid <- validateFrameResponse rsp
    if isValid == True
        then H.getResponseBody rsp >>= (\rsBody -> return $ Just rsBody)
        else putStrLn ("Invalid response for " ++ (show url)) >> return Nothing


validateFrameResponse :: Result (H.Response L.ByteString) -> IO Bool
validateFrameResponse rsp = do
    responseCode <- H.getResponseCode rsp
    if responseCode == (2,0,0)
        then return True
        else return False


simpleLazyRequest
    :: String                       -- ^URL to fetch
    -> H.Request L.ByteString       -- ^The constructed request
simpleLazyRequest urlString =
  case parseURI urlString of
    Nothing -> error ("simpleLazyRequest: Not a valid URL - " ++ urlString)
    Just u  -> H.mkRequest H.GET u



constructFrameURL :: String -> Int -> Int -> String
constructFrameURL pIp pPort seqNr =
    "http://" ++ pIp ++ ":" ++ (show pPort) ++ "/" ++ (show seqNr) ++ ".frame"


constructCounterURL :: String -> Int -> String
constructCounterURL pIp pPort =
    "http://" ++ pIp ++ ":" ++ (show pPort) ++ "/counter"


httpGetCounter :: String -> B.ByteString -> IO (Maybe (Int, B.ByteString))
httpGetCounter url etg = do
    maybeResp <- H.simpleHTTP req
    case maybeResp of
        Left _      -> return Nothing
        Right resp  -> do
            let etagValue = getETagFromResponse resp
            isValid <- validateCounterReponse resp
            if isValid == True
                then (assembleResult (maybeRead $ HB.rspBody resp) etagValue)
                else return Nothing
        where
            req = replaceHeader HdrIfNoneMatch (C.unpack etg) $ H.getRequest url
            assembleResult mCounter etagVal =
                case mCounter
                    of Just counterVal -> return $ Just (counterVal, etagVal)
                       Nothing    -> return $ Nothing


getETagFromResponse :: H.Response String -> B.ByteString
getETagFromResponse response =
    case etagValue of
        Just a -> C.pack a
        Nothing -> B.empty
    where
        etagValue = findHeader HdrETag response


validateCounterReponse :: H.Response String -> IO Bool
validateCounterReponse rsp = do
    let responseCode = HB.rspCode rsp
    if responseCode == (2,0,0)
        then return True
        else if responseCode == (3,0,4)
            then return False           -- still valid, but empty response
            else do putStrLn $ "Error getting counter. Response code: "
                               ++ (show responseCode)
                    return False