 {-# LANGUAGE FlexibleContexts #-}
 {-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.Net
  ( ipv4Field
  , ipv4RangeField
  , macField
  , NetFormMessage(..)
  , englishNetFormMessage
  ) where

import Data.Text (Text)
import Net.Types (IPv4, IPv4Range(..), Mac)
import Yesod.Core
import Yesod.Form.Fields
import Yesod.Form.Types

import qualified Net.IPv4 as IPv4
import qualified Net.Mac as Mac
import qualified Data.Attoparsec.Text as AT

data NetFormMessage
  = MsgInvalidIPv4
  | MsgInvalidIPv4Range
  | MsgInvalidMac

englishNetFormMessage :: NetFormMessage -> Text
englishNetFormMessage x = case x of
  MsgInvalidIPv4 -> "Please enter an IPv4 address in dot decimal notation."
  MsgInvalidIPv4Range -> "Please enter an IPv4 range in CIDR notation."
  MsgInvalidMac -> "Please enter a valid MAC address."

ipv4Field ::
  ( Monad m
  , RenderMessage (HandlerSite m) NetFormMessage
  , RenderMessage (HandlerSite m) FormMessage
  ) => Field m IPv4
ipv4Field = mapField IPv4.encode from textField
  where
  from t = case IPv4.decode t of
    Nothing -> Left (SomeMessage MsgInvalidIPv4)
    Just ipv4 -> Right ipv4

ipv4RangeField ::
  ( Monad m
  , RenderMessage (HandlerSite m) NetFormMessage
  , RenderMessage (HandlerSite m) FormMessage
  ) => Field m IPv4Range
ipv4RangeField = mapField IPv4.encodeRange from textField
  where
  from t = case IPv4.decodeRange t of
    Nothing -> Left (SomeMessage MsgInvalidIPv4Range)
    Just r -> Right r

-- | This is similar to 'ipv4RangeField'. However, it preserves the
-- original IPv4 address that the user enters rather than normalizing
-- the range. For example, on the user input @192.168.1.55/24@, the
-- functions return:
--
-- * 'unnormalizedIPv4RangeField': @'IPv4Range' ('ipv4' 192 168 1 55) 24@
-- * 'ipv4RangeField': @'IPv4Range' ('ipv4' 192 168 1 0) 24@
unnormalizedIPv4RangeField :: 
  ( Monad m
  , RenderMessage (HandlerSite m) NetFormMessage
  , RenderMessage (HandlerSite m) FormMessage
  ) => Field m IPv4Range
unnormalizedIPv4RangeField = mapField IPv4.encodeRange from textField
  where
  from t = case unnormalizedDecodeRange t of
    Nothing -> Left (SomeMessage MsgInvalidIPv4Range)
    Just r -> Right r

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

macField :: ( Monad m
            , RenderMessage (HandlerSite m) NetFormMessage
            , RenderMessage (HandlerSite m) FormMessage
            ) => Field m Mac
macField = mapField Mac.encode from textField
  where
  from t = case Mac.decode t of
    Nothing -> Left (SomeMessage MsgInvalidMac)
    Just mac -> Right mac

mapField :: Monad m => (a -> b) -> (b -> Either (SomeMessage (HandlerSite m)) a) -> Field m b -> Field m a
mapField fwd bck (Field parse view enctype) = Field
  (\ts fis -> do
     eres <- parse ts fis
     return $ eres >>= (\mb -> case mb of
       Just b  -> Just <$> bck b
       Nothing -> Right Nothing)
  )
  (\a b c d e -> view a b c (fmap fwd d) e)
  enctype

