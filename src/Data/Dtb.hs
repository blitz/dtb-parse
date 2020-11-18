module Data.Dtb
  (parseDtb, Dtb(..), MemoryReservation(..), Property(..), DeviceTree(..))
where

import           Data.ByteString   as B
import           Data.Dtb.LowLevel
import           Data.Dtb.Parser

-- |The representation of a device tree.
--
-- Memory reservations are not part of the tree structure itself, but
-- are represented using a list. Everything else exists as a
-- `DeviceTree` tree.
data Dtb = Dtb [MemoryReservation] DeviceTree

-- |Parses a device tree from a `ByteString`.
--
-- Returns Nothing on failure.
parseDtb :: B.ByteString -> Maybe Dtb
parseDtb dta = do
  header <- parseHeader dta
  memRsv <- memoryReservations header dta
  sb <- stringsBlock header dta
  stb <- structBlock header dta
  tokens <- deviceTreeTokens sb stb
  tree <- parse tokens
  return $ Dtb memRsv tree
