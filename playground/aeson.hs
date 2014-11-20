{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Data.Aeson
import Control.Monad
import Control.Applicative


data Neighbor = Neighbor
    { ip :: Text
    , port  :: Int
    , revision :: Int
    } deriving Show


instance FromJSON Neighbor where
    parseJSON (Object v) = Neighbor <$>
                           v .: "ip" <*>
                           v .: "port" <*>
                           v .: "revision"

    parseJSON _          = mzero


main = do
    let a = decode "{\"ip\":\"127.0.0.1\",\"port\":12, \"revision\":1}" :: Maybe Neighbor
    print a