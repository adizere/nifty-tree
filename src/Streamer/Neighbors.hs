{-# LANGUAGE OverloadedStrings #-}

module Streamer.Neighbors where


import Data.Aeson
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Lazy as BL


data Neighbor = Neighbor
    { nbIp              :: String
    , nbPort            :: Int
    , nbCycleNumber     :: Int
    , nbCycleDirection  :: Int
    } deriving (Eq, Show)


instance FromJSON Neighbor where
    parseJSON (Object v) = Neighbor <$>
                           v .: "nbIp" <*>
                           v .: "nbPort" <*>
                           v .: "nbCycleNumber" <*>
                           v .: "nbCycleDirection"

    parseJSON _ = mzero


data OverlayInfo = OverlayInfo
    { oiNeighbors   :: [Neighbor]
    , oiRevision    :: Int
    , oiSourceIp    :: String
    , oiSourcePort  :: Int
    } deriving (Eq, Show)


instance FromJSON OverlayInfo where
    parseJSON (Object v) = OverlayInfo <$>
                          v .: "oiNeighbors" <*>
                          v .: "oiRevision" <*>
                          v .: "oiSourceIp" <*>
                          v .: "oiSourcePort"

    parseJSON _ = mzero


maybeGetOverlayInfo :: FilePath -> IO (Maybe OverlayInfo)
maybeGetOverlayInfo sessionFile = do
    toParse <- BL.readFile sessionFile
    return (decode $ toParse :: Maybe OverlayInfo)