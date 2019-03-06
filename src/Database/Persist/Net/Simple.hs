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

import Data.Monoid
import Data.Text (Text)
import Database.Persist
import Database.Persist.Class
import Database.Persist.Sql
import Net.Types (IPv4, Mac)

import qualified Data.Text as Text
import qualified Net.IPv4 as IPv4
import qualified Net.Mac as Mac

instance PersistField IPv4 where
  toPersistValue = toPersistValueTextShow IPv4.encode
  fromPersistValue = fromPersistValueTextRead IPv4.decode

instance PersistFieldSql IPv4 where
  sqlType _ = SqlString

instance PersistField Mac where
  toPersistValue = toPersistValueTextShow Mac.encode
  fromPersistValue = fromPersistValueTextRead Mac.decode

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

