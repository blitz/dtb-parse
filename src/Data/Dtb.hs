module Data.Dtb
  (MemoryReservation(..), Property(..), DeviceTree(..))
where

import           Data.ByteString   as B
import           Data.Dtb.LowLevel
import           Data.Dtb.Parser

-- TODO Documentation

data Dtb = Dtb [MemoryReservation] DeviceTree

parseDtb :: B.ByteString -> Maybe Dtb
parseDtb dta = do
  header <- parseHeader dta
  memRsv <- memoryReservations header dta
  sb <- stringsBlock header dta
  stb <- structBlock header dta
  tokens <- deviceTreeTokens sb stb
  tree <- parse tokens
  return $ Dtb memRsv tree
