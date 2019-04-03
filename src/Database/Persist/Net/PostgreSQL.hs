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

import Data.Monoid
import Data.Text (Text)
import Database.Persist
import Database.Persist.Class
import Database.Persist.Sql
import Net.IPv4.Unnormalized (unnormalizedDecodeRange)
import Net.Types (IPv4,IPv4Range,Mac)

import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Net.IPv4 as IPv4
import qualified Net.Mac as Mac

instance PersistField IPv4 where
  toPersistValue = toPersistValue . IPv4.encode
  fromPersistValue v = case v of
    PersistDbSpecific s -> case IPv4.decodeUtf8 s of
      Just x -> Right x
      Nothing -> Left (Text.pack "PersistValue IPv4: Invalid format")
    PersistText t -> case IPv4.decode t of
      Just x -> Right x
      Nothing -> Left (Text.pack "PersistValue IPv4: Invalid format")
    y -> Left $ Text.pack "PersistValue IPv4: Not a PersistDbSpecific: " <> Text.pack (show y)

-- | This does not normalize the range. Since PostgreSQL allows
-- the user to store nonnormalized ranges, this instance preserves
-- this behavior.
instance PersistField IPv4Range where
  toPersistValue = toPersistValue . IPv4.encodeRange
  fromPersistValue v = case v of
    PersistDbSpecific s -> case TE.decodeUtf8' s of
      Right t -> case unnormalizedDecodeRange t of
        Just x -> Right x
        Nothing -> Left (Text.pack "PersistValue IPv4Range: Invalid format")
      Left _ -> Left (Text.pack "PersistValue IPv4Range: Invalid format")
    PersistText t -> case unnormalizedDecodeRange t of
      Just x -> Right x
      Nothing -> Left (Text.pack "PersistValue IPv4Range: Invalid format")
    y -> Left $ Text.pack "PersistValue IPv4: Not a PersistDbSpecific: " <> Text.pack (show y)
  

instance PersistFieldSql IPv4 where
  sqlType _ = SqlOther (Text.pack "inet")

instance PersistFieldSql IPv4Range where
  sqlType _ = SqlOther (Text.pack "inet")

instance PersistField Mac where
  toPersistValue = toPersistValue . Mac.encode
  fromPersistValue v = case v of
    PersistDbSpecific s -> case Mac.decodeUtf8 s of
      Just x -> Right x
      Nothing -> Left (Text.pack "PersistValue MAC: Invalid format")
    PersistText t -> case Mac.decode t of
      Just x -> Right x
      Nothing -> Left (Text.pack "PersistValue MAC: Invalid format")
    y -> Left $ Text.pack "PersistValue MAC: Not a PersistDbSpecific: " <> Text.pack (show y)

instance PersistFieldSql Mac where
  sqlType _ = SqlOther (Text.pack "macaddr")
