{-# LANGUAGE OverloadedStrings #-}

module Streamer.Neighbors where


import Data.Text
import Data.Aeson
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Lazy as BL


data Neighbor = Neighbor
    { neighborIp :: Text
    , neighborPort  :: Int
    , cycleNumber   :: Int
    , cycleDirection :: Int
    } deriving (Eq, Show)


instance FromJSON Neighbor where
    parseJSON (Object v) = Neighbor <$>
                           v .: "neighborIp" <*>
                           v .: "neighborPort" <*>
                           v .: "cycleNumber" <*>
                           v .: "cycleDirection"

    parseJSON _ = mzero


data NeighborList = NeighborList
    { list     :: [Neighbor]
    , neighborListRevision :: Int
    } deriving (Eq, Show)


instance FromJSON NeighborList where
    parseJSON (Object v) = NeighborList <$>
                          v .: "list" <*>
                          v .: "neighborListRevision"

    parseJSON _ = mzero


getAllNeighboringNodes :: FilePath -> IO (Maybe NeighborList)
getAllNeighboringNodes sessionFile = do
    toParse <- BL.readFile sessionFile
    return (decode $ toParse :: Maybe NeighborList)