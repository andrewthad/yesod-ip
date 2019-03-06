-- | This module provides orphan instances for 'PathPiece'
--   for data types from the @ip@ package.

module Web.PathPieces.Net where

import Data.Monoid
import Data.Text (Text)
import Net.Types (IPv4,Mac)
import Web.PathPieces (PathPiece(..))

import qualified Net.IPv4 as IPv4
import qualified Net.Mac as Mac

instance PathPiece Mac where
  toPathPiece   = Mac.encode
  fromPathPiece = Mac.decode

instance PathPiece IPv4 where
  toPathPiece   = IPv4.encode
  fromPathPiece = IPv4.decode

