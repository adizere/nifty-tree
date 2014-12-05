import Network.HTTP
import Network.URI ( parseURI )
import Network.Stream (Result)
import qualified Data.ByteString.Lazy as Lazy


main = do
    result <- doRequest "http://localhost/3.frame"
    case result of Just bytes -> putStrLn (show bytes)
                   otherwise -> putStrLn "Error executing request"


doRequest :: String -> IO (Maybe Lazy.ByteString)
doRequest url = do
    rsp <- simpleHTTP $ getLazyRequest url
    isValid <- validateReponse rsp
    if isValid == True
        then getResponseBody rsp >>= (\rsBody -> return $ Just rsBody)
        else return Nothing


validateReponse :: Result (Response Lazy.ByteString) -> IO Bool
validateReponse rsp = do
    rspCode <- getResponseCode rsp
    if rspCode == (2,0,0)
        then return True
        else do
            putStrLn $ "Error executing request. Response code: "
                       ++ (show rspCode)
            return False


getLazyRequest
    :: String                   -- ^URL to fetch
    -> Request Lazy.ByteString  -- ^The constructed request
getLazyRequest urlString =
  case parseURI urlString of
    Nothing -> error ("getLazyRequest: Not a valid URL - " ++ urlString)
    Just u  -> mkRequest GET u