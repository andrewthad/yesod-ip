module Net.IPv4.Unnormalized
  ( unnormalizedDecodeRange
  ) where

import Data.Text (Text)
import Net.Types (IPv4, IPv4Range(..), Mac)

import qualified Net.IPv4 as IPv4
import qualified Data.Attoparsec.Text as AT

-- Decode an 'IPv4Range' from 'Text'.
unnormalizedDecodeRange :: Text -> Maybe IPv4Range
unnormalizedDecodeRange =
  either (const Nothing) Just . AT.parseOnly (unnormalizedParserRange <* AT.endOfInput)

-- The same this as parserRange except that it does not
-- do any normalization of the range.
unnormalizedParserRange :: AT.Parser IPv4Range
unnormalizedParserRange = do
  ip <- IPv4.parser
  _ <- AT.char '/'
  theMask <- AT.decimal >>= limitSize
  return (IPv4Range ip theMask)
  where
  limitSize i =
    if i > 32
      then fail "An IPv4 range length must be between 0 and 32"
      else return i
