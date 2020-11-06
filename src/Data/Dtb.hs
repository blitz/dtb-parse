module Data.Dtb (Header(..), parseHeader)
where

import           Data.Binary.Get
import           Data.ByteString      as B
import           Data.ByteString.Lazy as BL
import           Data.Word            (Word32)

-- |The header of a flattened device tree file.
--
-- See Section 5.2 "Header" in the specification.
data Header = Header
  { magic             :: !Word32
  , totalsize         :: !Word32
  , off_dt_struct     :: !Word32
  , off_dt_strings    :: !Word32
  , off_mem_rsvmap    :: !Word32
  , version           :: !Word32
  , last_comp_version :: !Word32
  , boot_cpuid_phys   :: !Word32
  , size_dt_strings   :: !Word32
  , size_dt_struct    :: !Word32
  }

headerMagic :: Word32
headerMagic = 0xd00dfeed

headerParser :: Get Header
headerParser = Header
               <$> getWord32be
               <*> getWord32be
               <*> getWord32be
               <*> getWord32be
               <*> getWord32be
               <*> getWord32be
               <*> getWord32be
               <*> getWord32be
               <*> getWord32be
               <*> getWord32be

-- |Parse a DTB header from a byte string.
parseHeader :: B.ByteString -> Maybe Header
parseHeader dta = case runGetOrFail headerParser (BL.fromStrict dta) of
  Left (_, _, errorMsg) -> Nothing
  Right (_, _, header)  -> Just header

-- Nothing here yet.
