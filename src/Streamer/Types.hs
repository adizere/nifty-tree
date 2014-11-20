{-# LANGUAGE OverloadedStrings #-}

module Streamer.Types where

import qualified Data.ByteString.Lazy   as L
import Data.Text


data PullNode = PullNode
    { ip        :: String
    , port      :: Int
    } deriving (Eq, Show)


data PullNodesList = PullNodesList
    { activeNode    :: PullNode
    , backupNodes   :: [PullNode]
    , revision      :: Int
    } deriving (Eq, Show)


data Frame = Frame
    { seqNr     :: Int
    , digest    :: String
    , content   :: L.ByteString
    } deriving (Eq, Show)