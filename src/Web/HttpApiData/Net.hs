-- | This module provides orphan instances for 'ToHttpApiData'
--   and 'FromHttpApiData' for data types from the @ip@ package.

module Web.HttpApiData.Net () where

import Data.Monoid
import Data.Text (Text)
import Net.Types (IPv4,Mac)
import Web.HttpApiData (ToHttpApiData(..),FromHttpApiData(..))

import qualified Data.Text as Text
import qualified Net.IPv4 as IPv4
import qualified Net.Mac as Mac

instance ToHttpApiData Mac where
  toUrlPiece   = Mac.encode
  toHeader     = Mac.encodeUtf8
  toQueryParam = Mac.encode

instance FromHttpApiData Mac where
  parseUrlPiece   = describeError mac . Mac.decode
  parseQueryParam = describeError mac . Mac.decode
  parseHeader     = describeError mac . Mac.decodeUtf8

instance ToHttpApiData IPv4 where
  toUrlPiece   = IPv4.encode
  toHeader     = IPv4.encodeUtf8
  toQueryParam = IPv4.encode

instance FromHttpApiData IPv4 where
  parseUrlPiece   = describeError ipv4 . IPv4.decode
  parseQueryParam = describeError ipv4 . IPv4.decode
  parseHeader     = describeError ipv4 . IPv4.decodeUtf8

mac,ipv4 :: Text
mac = Text.pack "MAC Address"
ipv4 = Text.pack "IPv4 Address"

describeError :: Text -> Maybe a -> Either Text a
describeError name x = case x of
  Nothing -> Left (Text.pack "could not parse " <> name)
  Just a  -> Right a

