-- | This module provides orphan instances for 'PathPiece'
--   for data types from the @ip@ package.

module Web.PathPieces.Net where

import Web.PathPieces (PathPiece(..))
import Data.Text (Text)
import Data.Monoid
import Net.IPv4 (IPv4)
import Net.Mac (Mac)
import qualified Net.IPv4.Text as IPv4Text
import qualified Net.Mac.Text as MacText

instance PathPiece Mac where
  toPathPiece   = MacText.encode
  fromPathPiece = MacText.decode

instance PathPiece IPv4 where
  toPathPiece   = IPv4Text.encode
  fromPathPiece = IPv4Text.decode

