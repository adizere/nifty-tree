-- {-# LANGUAGE OverloadedStrings #-}

module Streamer.Types where


import qualified Data.ByteString.Lazy   as L


data Frame = Frame
    { frSeqNr     :: Int
    , frDigest    :: String
    , frContent   :: L.ByteString
    } deriving (Eq, Show)