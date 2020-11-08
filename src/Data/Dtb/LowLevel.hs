{-# LANGUAGE Safe #-}
module Data.Dtb.LowLevel
  (Header(..), MemoryReservation(..),
    parseHeader, stringsBlock, structBlock, memoryReservations)
where

import           Control.Monad        (guard)
import           Data.Binary.Get
import           Data.ByteString      as B
import           Data.ByteString.Lazy as BL
import           Data.Word            (Word32, Word64)

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
  deriving (Eq, Show)

data MemoryReservation = MemoryReservation
  { address :: !Word64
  , size    :: !Word64
  }
  deriving (Eq, Show)

type RawDtbData = B.ByteString
type StringsBlock = B.ByteString
type StructureBlock = B.ByteString

headerMagic :: Word32
headerMagic = 0xd00dfeed

headerSupportedVersions :: [Word32]
headerSupportedVersions = [16, 17]

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

isValidHeader :: Header -> Bool
isValidHeader header =
  magic header == headerMagic && last_comp_version header `Prelude.elem` headerSupportedVersions

runParserOrFail :: Get a -> B.ByteString -> Maybe a
runParserOrFail p dta = case runGetOrFail p (BL.fromStrict dta) of
  Left (_, _, _errorMsg)  -> Nothing
  Right (_, _, parsedVal) -> Just parsedVal

-- |Parse a DTB header from a byte string.
--
-- Returns `Nothing`, if the header is not recognized as valid.
parseHeader :: RawDtbData -> Maybe Header
parseHeader dta = do
  header <- runParserOrFail headerParser dta
  guard $ isValidHeader header
  return header

memoryReservationParser :: Get MemoryReservation
memoryReservationParser = MemoryReservation
                          <$> getWord64be
                          <*> getWord64be

memoryReservationsParser :: Get [MemoryReservation]
memoryReservationsParser = do
  reservation <- memoryReservationParser
  case reservation of
    -- Memory reservations are terminated by a special token.
    MemoryReservation 0 0 -> return []
    _ -> do
      rest <- memoryReservationsParser
      return $ reservation:rest

-- |Return a list of memory reservations from a DTB.
memoryReservations :: Header -> RawDtbData -> Maybe [MemoryReservation]
memoryReservations header = runParserOrFail memoryReservationsParser
                            . B.take (fromIntegral $ off_mem_rsvmap header)

-- |Slice a block out of a byte string given by start and length.
sliceBlock :: Word32 -> Word32 -> RawDtbData -> Maybe B.ByteString
sliceBlock start size dta = do
  guard $ B.length block == (fromIntegral size)
  return block
  where block = B.take (fromIntegral size)
                $ B.drop (fromIntegral start) dta

-- |Extract the strings block from a DTB.
--
-- This function may fail with `Nothing` if the DTB is malformed.
stringsBlock :: Header -> RawDtbData -> Maybe StringsBlock
stringsBlock header = sliceBlock (off_dt_strings header) (size_dt_strings header)

-- |Extract the strings block from a DTB.
--
-- This function may fail with `Nothing` if the DTB is malformed.
structBlock :: Header -> RawDtbData -> Maybe StructureBlock
structBlock header = sliceBlock (off_dt_struct header) (size_dt_struct header)
