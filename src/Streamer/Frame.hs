module Streamer.Frame where


import qualified Codec.Digest.SHA       as S
import qualified Data.ByteString.Lazy   as L


data Frame = Frame
    { frSeqNr     :: Int
    , frDigest    :: String
    , frContent   :: L.ByteString
    } deriving (Eq, Show)


verifyDigest :: String -> L.ByteString -> Bool
verifyDigest digest content =
    digest == S.showBSasHex (S.hash S.SHA256 content)