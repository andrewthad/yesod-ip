 {-# LANGUAGE OverloadedStrings #-}

-- | This module provides orphan instances for the typeclasses
--   'PersistField' and 'PersistFieldSql'. The instances provided
--   are for the data types 'IPv4' and 'Mac' from the @ip@ package. 
--   These instances will choose the 
--   standard text type for the database column. If you are
--   using PostgreSQL, you may want to consider importing 
--   the @Database.Persist.Net.PostgreSQL@ module instead.
module Database.Persist.Net.Simple 
  () where

import Database.Persist
import Database.Persist.Class
import Database.Persist.Sql

import Data.Text (Text)
import Data.Monoid
import Net.IPv4 (IPv4)
import Net.Mac (Mac)
import qualified Data.Text as Text
import qualified Net.IPv4.Text as IPv4Text
import qualified Net.Mac.Text as MacText

instance PersistField IPv4 where
  toPersistValue = toPersistValueTextShow IPv4Text.encode
  fromPersistValue = fromPersistValueTextRead IPv4Text.decode

instance PersistFieldSql IPv4 where
  sqlType _ = SqlString

instance PersistField Mac where
  toPersistValue = toPersistValueTextShow MacText.encode
  fromPersistValue = fromPersistValueTextRead MacText.decode

instance PersistFieldSql Mac where
  sqlType _ = SqlString

fromPersistValueTextRead :: (Text -> Maybe a) -> PersistValue -> Either Text a
fromPersistValueTextRead fromText z = do
  t <- fromPersistValueText z
  case fromText t of
    Nothing -> Left $ "Could not parse the following text:" <> Text.pack (show t)
    Just v -> Right v

toPersistValueTextShow :: (a -> Text) -> a -> PersistValue
toPersistValueTextShow f a = PersistText (f a)

