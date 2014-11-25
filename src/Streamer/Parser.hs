{-# LANGUAGE OverloadedStrings #-}

module Streamer.Parser where

import Streamer.Types

import Data.Aeson
import Control.Monad
import Control.Applicative


instance FromJSON PullNode where
    parseJSON (Object v) = PullNode <$>
                           v .: "ip" <*>
                           v .: "port"

    parseJSON _          = mzero