-- {-# LANGUAGE OverloadedStrings #-}

module Streamer.Types where

import qualified Data.ByteString.Lazy   as L


data Frame = Frame
    { seqNr     :: Int
    , digest    :: String
    , content   :: L.ByteString
    } deriving (Eq, Show)