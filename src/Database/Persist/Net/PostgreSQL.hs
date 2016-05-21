-- | This module provides orphan instances for data types
--   from the @ip@ package. These instances only work for
--   PostgresSQL. The following PostgreSQL column types are
--   used for each data types:
--   
--   * 'IPv4': @inet@
--   * 'Mac': @macaddr@
--   
--   This module is not yet complete.
module Database.Persist.Net.PostgreSQL 
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


