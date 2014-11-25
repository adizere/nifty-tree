{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Data.Aeson
import Control.Monad
import Control.Applicative


data Neighbor = Neighbor
    { ip :: Text
    , port  :: Int
    , revision :: Int
    } deriving (Eq, Show)


instance FromJSON Neighbor where
    parseJSON (Object v) = Neighbor <$>
                           v .: "ip" <*>
                           v .: "port" <*>
                           v .: "revision"

    parseJSON _          = mzero

data NeighborList = NeighborList
  { list :: [Neighbor]
  } deriving (Eq, Show)


instance FromJSON NeighborList where
  parseJSON (Object v) = NeighborList <$>
                          v .: "list"


main = do
    -- let a = decode "{\"ip\":\"127.0.0.1\",\"port\":12, \"revision\":1}" :: Maybe Neighbor
    let a = decode "{\"list\":[{\"ip\":\"127.0.0.1\",\"port\":12,\"revision\":1},{\"ip\":\"127.0.0.2\",\"port\":13,\"revision\":1},{\"ip\":\"127.0.0.3\",\"port\":14,\"revision\":1}]}" :: Maybe NeighborList
    print a