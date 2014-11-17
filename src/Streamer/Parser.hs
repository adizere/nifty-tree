module Streamer.Parser where

import Data.Aeson
import Streamer.Parser

-- instance FromJSON PullNode where
--     parseJSON (Object r) = PullNode <$>
--                             r .: "revision"