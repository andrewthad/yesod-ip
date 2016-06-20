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

import           Database.Persist
import           Database.Persist.Class
import           Database.Persist.Sql

import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Net.IPv4                  (IPv4)
import qualified Net.IPv4.ByteString.Char8 as IPv4ByteString
import qualified Net.IPv4.Text             as IPv4Text
import           Net.Mac                   (Mac)
import qualified Net.Mac.ByteString.Char8  as MacByteString
import qualified Net.Mac.Text              as MacText

instance PersistField IPv4 where
  toPersistValue = toPersistValue . IPv4Text.encode
  fromPersistValue v = case v of
    PersistDbSpecific s -> case IPv4ByteString.decode s of
      Just x -> Right x
      Nothing -> Left (Text.pack "PersistValue IPv4: Invalid format")
    y -> Left $ Text.pack "PersistValue IPv4: Not a PersistDbSpecific: " <> Text.pack (show y)

instance PersistFieldSql IPv4 where
  sqlType _ = SqlOther (Text.pack "inet")

instance PersistField Mac where
  toPersistValue = toPersistValue . MacText.encode
  fromPersistValue v = case v of
    PersistDbSpecific s -> case MacByteString.decode s of
      Just x -> Right x
      Nothing -> Left (Text.pack "PersistValue MAC: Invalid format")
<<<<<<< HEAD
    y -> Left $ Text.pack "PersistValue MAC: Not a PersistDbSpecific: " <> Text.pack (show y)
=======
    y -> Left $ Text.pack "PersistValue MAC: Not a PersistDbSpecific " <> Text.pack (show y)
>>>>>>> 4dfb9b2397e3840525c364bdf2aa62f669f46e71

instance PersistFieldSql Mac where
  sqlType _ = SqlOther (Text.pack "macaddr")
