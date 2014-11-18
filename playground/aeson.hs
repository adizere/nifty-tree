{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Data.Aeson
-- import Data.Functor
import Control.Monad
import Control.Applicative


data Person = Person
    { name :: Text
    , age  :: Int
    } deriving Show


instance FromJSON Person where
    parseJSON (Object v) = Person <$>
                           v .: "name" <*>
                           v .: "age"

    parseJSON _          = mzero


main = do
    let a = decode "{\"name\":\"Joe\",\"age\":12}" :: Maybe Person
    print a