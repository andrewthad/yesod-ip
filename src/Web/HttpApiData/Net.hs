-- | This module provides orphan instances for 'ToHttpApiData'
--   and 'FromHttpApiData' for data types from the @ip@ package.

module Web.HttpApiData.Net () where

import Web.HttpApiData (ToHttpApiData(..),FromHttpApiData(..))
import Data.Text (Text)
import Data.Monoid
import Net.IPv4 (IPv4)
import Net.Mac (Mac)
import qualified Data.Text as Text
import qualified Net.IPv4.Text as IPv4Text
import qualified Net.IPv4.ByteString.Char8 as IPv4ByteString
import qualified Net.Mac.Text as MacText
import qualified Net.Mac.ByteString.Char8 as MacByteString

instance ToHttpApiData Mac where
  toUrlPiece   = MacText.encode
  toHeader     = MacByteString.encode
  toQueryParam = MacText.encode

instance FromHttpApiData Mac where
  parseUrlPiece   = describeError mac . MacText.decode
  parseQueryParam = describeError mac . MacText.decode
  parseHeader     = describeError mac . MacByteString.decode

instance ToHttpApiData IPv4 where
  toUrlPiece   = IPv4Text.encode
  toHeader     = IPv4ByteString.encode
  toQueryParam = IPv4Text.encode

instance FromHttpApiData IPv4 where
  parseUrlPiece   = describeError ipv4 . IPv4Text.decode
  parseQueryParam = describeError ipv4 . IPv4Text.decode
  parseHeader     = describeError ipv4 . IPv4ByteString.decode

mac,ipv4 :: Text
mac = Text.pack "MAC Address"
ipv4 = Text.pack "IPv4 Address"

describeError :: Text -> Maybe a -> Either Text a
describeError name x = case x of
  Nothing -> Left (Text.pack "could not parse " <> name)
  Just a  -> Right a

