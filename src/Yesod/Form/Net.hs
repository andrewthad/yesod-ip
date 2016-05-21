 {-# LANGUAGE FlexibleContexts #-}
 {-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.Net 
  ( ipv4Field
  , macField
  , NetFormMessage(..)
  ) where

import Yesod.Core
import Yesod.Form.Fields
import Yesod.Form.Types
import Net.IPv4 (IPv4)
import Net.Mac (Mac)
import Data.Text (Text)
import qualified Net.IPv4 as IPv4
import qualified Net.Mac as Mac

data NetFormMessage
  = MsgInvalidIPv4
  | MsgInvalidMac

englishNetFormMessage :: NetFormMessage -> Text
englishNetFormMessage x = case x of
  MsgInvalidIPv4 -> "Please enter an IPv4 address in dot decimal notation." 
  MsgInvalidMac -> "Please enter a valid MAC address."

ipv4Field :: ( Monad m
             , RenderMessage (HandlerSite m) NetFormMessage 
             , RenderMessage (HandlerSite m) FormMessage 
             ) => Field m IPv4
ipv4Field = mapField IPv4.toDotDecimalText from textField
  where 
  from t = case IPv4.fromDotDecimalText t of
    Nothing -> Left (SomeMessage MsgInvalidIPv4)
    Just ipv4 -> Right ipv4

macField :: ( Monad m
            , RenderMessage (HandlerSite m) NetFormMessage 
            , RenderMessage (HandlerSite m) FormMessage 
            ) => Field m Mac
macField = mapField Mac.toText from textField
  where 
  from t = case Mac.fromText t of
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

