{-# LANGUAGE Safe #-}
module Data.Dtb.LowLevel
  (Header(..), MemoryReservation(..), Token(..), PropData,
    parseHeader, stringsBlock, structBlock, memoryReservations, extractString,
    deviceTreeTokens)
where

import           Control.Monad            (guard, void)
import           Data.Binary.Get
import           Data.ByteString          as B
import           Data.ByteString.Lazy     as BL
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as E
import qualified Data.Text.Encoding.Error as E
import           Data.Word                (Word32, Word64)

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

-- TODO There is also the binary-strict package, which should handle
-- strict bytestrings.
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

-- |Get a list of items by repeatedly applying a parser until an exit
-- condition is met.
getDelimitedList :: Get a -> (a -> Bool) -> Get [a]
getDelimitedList p pred = do
  it <- p
  if pred it then return [] else do
    rest <- getDelimitedList p pred
    return $ it:rest

memoryReservationsParser :: Get [MemoryReservation]
memoryReservationsParser =
  getDelimitedList memoryReservationParser (== (MemoryReservation 0 0))

-- |Return a list of memory reservations from a DTB.
memoryReservations :: Header -> RawDtbData -> Maybe [MemoryReservation]
memoryReservations header = runParserOrFail memoryReservationsParser
                            . B.drop (fromIntegral $ off_mem_rsvmap header)

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

-- |Extract a string from the strings block.
--
-- The specification doesn't specify the encoding, so we assume it's
-- UTF-8. Invalid UTF-8 or invalid offsets will result in `Nothing`.
extractString :: StringsBlock -> Word32 -> Maybe T.Text
extractString sb o
  | fromIntegral o >= B.length sb = Nothing
  | otherwise =  case E.decodeUtf8' $ B.takeWhile (/= 0) $ B.drop (fromIntegral o) sb of
                   Left _  -> Nothing
                   Right t -> Just t

-- |Extract the strings block from a DTB.
--
-- This function may fail with `Nothing` if the DTB is malformed.
structBlock :: Header -> RawDtbData -> Maybe StructureBlock
structBlock header = sliceBlock (off_dt_struct header) (size_dt_struct header)

type PropData = B.ByteString

data Token = BeginNode T.Text
           | EndNode
           | Prop T.Text PropData
           | Nop
           | End
  deriving (Eq, Show, Ord)

-- |Parse a zero-terminated string.
--
-- This parser makes sure that a 32-bit aligned chunk of input is
-- consumed.
alignedStringParser :: Get T.Text
alignedStringParser = do
  string <- getDelimitedList getWord8 (== 0)
  void $ getByteString $ 3 - (Prelude.length string `mod` 4)
  return $ E.decodeUtf8With E.lenientDecode $ B.pack string

beginNodeParser :: Get Token
beginNodeParser = BeginNode <$> alignedStringParser

propParser :: StringsBlock -> Get Token
propParser sb = do
  len <- getWord32be
  nameoff <- getWord32be
  payload <- getByteString $ fromIntegral len
  void $ getByteString $ fromIntegral $ (4 - (len `mod` 4)) `mod` 4
  case extractString sb nameoff of
    Just name -> return $ Prop name payload
    Nothing   -> fail "invalid name offset"

tokenParser :: StringsBlock -> Get Token
tokenParser strs = do
  tokenType <- getWord32be
  case tokenType of
    0x1 -> beginNodeParser
    0x3 -> propParser strs
    0x2 -> return $ EndNode
    0x4 -> return $ Nop
    0x9 -> return $ End
    _   -> fail "Invalid token type"

tokensParser :: StringsBlock -> Get [Token]
tokensParser sb = do
  l <- getDelimitedList (tokenParser sb) (== End)
  return $ l ++ [End]

-- |Parse the structure block of the device tree into individual
-- tokens.
deviceTreeTokens :: StringsBlock -> StructureBlock -> Maybe [Token]
deviceTreeTokens = runParserOrFail . tokensParser
