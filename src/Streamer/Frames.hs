module Streamer.Frames where


import qualified Codec.Digest.SHA       as S
import qualified Data.ByteString.Lazy   as L


framesPersistPrefix :: String
framesPersistPrefix = "/opt/streamer/"


verifyDigest :: String -> L.ByteString -> Bool
verifyDigest "" _           = False
verifyDigest digest content
    | content == L.empty = False
    | otherwise          = digest == S.showBSasHex (S.hash S.SHA256 content)


persistFrame :: Int -> L.ByteString -> IO ()
persistFrame seqNr content = do
    putStrLn $ "Persisting frame "
             ++ (show $ seqNr) ++ " in "
             ++ (getFramePersistPath $ seqNr)
    L.writeFile (getFramePersistPath seqNr) content


getFramePersistPath :: Int -> FilePath
getFramePersistPath seqNr =
    framesPersistPrefix ++ (show seqNr) ++ ".frame"