-- | This module provides orphan instances for data types
--   from the @ip@ package. These instances only work for
--   PostgresSQL. The following PostgreSQL column types are
--   used for each data types:
--
--   * 'IPv4': @inet@
--   * 'Mac': @macaddr@
--
module Database.Persist.Net.PostgreSQL
  () where

import Database.Persist
import Database.Persist.Class
import Database.Persist.Sql

import Data.Text (Text)
import Data.Monoid
import Net.Types (IPv4,Mac)
import qualified Data.Text as Text
import qualified Net.IPv4.Text as IPv4Text
import qualified Net.IPv4.ByteString.Char8 as IPv4ByteString
import qualified Net.Mac.Text as MacText
import qualified Net.Mac.ByteString.Char8 as MacByteString

instance PersistField IPv4 where
  toPersistValue = toPersistValue . IPv4Text.encode
  fromPersistValue v = case v of
    PersistDbSpecific s -> case IPv4ByteString.decode s of
      Just x -> Right x
      Nothing -> Left (Text.pack "PersistValue IPv4: Invalid format")
    _ -> Left (Text.pack "PersistValue IPv4: Not a PersistDbSpecific")

instance PersistFieldSql IPv4 where
  sqlType _ = SqlOther (Text.pack "inet")

instance PersistField Mac where
  toPersistValue = toPersistValue . MacText.encode
  fromPersistValue v = case v of
    PersistDbSpecific s -> case MacByteString.decode s of
      Just x -> Right x
      Nothing -> Left (Text.pack "PersistValue MAC: Invalid format")
    _ -> Left (Text.pack "PersistValue MAC: Not a PersistDbSpecific")

instance PersistFieldSql Mac where
  sqlType _ = SqlOther (Text.pack "macaddr")



