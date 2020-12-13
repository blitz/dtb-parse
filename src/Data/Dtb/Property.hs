module Data.Dtb.Property
  (Property(..), propName, propData, asText, asWord32, asWord64, asRegList)
where

import           Data.Binary.Get
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Dtb.LowLevel        as D
import           Data.Either.Combinators  (rightToMaybe)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as E
import qualified Data.Text.Encoding.Error as E
import           Data.Word                (Word32, Word64)

-- |A property in a device tree.
data Property = Property !T.Text !D.PropData
  deriving (Show, Eq)

-- |Returns the property name.
propName :: Property -> T.Text
propName (Property name _) = name

-- |Returns the uninterpreted property data payload.
propData :: Property -> D.PropData
propData (Property _ dta) = dta

-- TODO When bytestring 0.10.12 is in stackage this can be implemented
-- using B.dropWhileEnd, which is a bit more efficient.
dropTrailingZeros :: D.PropData -> D.PropData
dropTrailingZeros = B.takeWhile (/= 0)

-- |Interpret property data as text.
asText :: Property -> T.Text
asText = E.decodeUtf8With E.lenientDecode . dropTrailingZeros . propData

-- |Interpret property data as a 32-bit word.
asWord32 :: Property -> Maybe Word32
asWord32 (Property _ dta) = case runGetOrFail getWord32be (BL.fromStrict dta) of
  Right (_, _, w) -> Just w
  Left e          -> Nothing

-- |Interpret property data a a 64-bit word.
asWord64 :: Property -> Maybe Word64
asWord64 (Property _ dta) = case runGetOrFail getWord64be (BL.fromStrict dta) of
  Right (_, _, w) -> Just w
  Left e          -> Nothing

-- |Parse a list of elements that goes on until the end of the input
-- data.
getList :: Get a -> Get [a]
getList p = do
  e <- isEmpty
  if e
    then return []
    else (do
             v <- p
             r <- getList p
             return $ v:r)

-- |Interpret a resource list.
--
-- This is usually a list of address size pairs. When size has zero
-- length, it will be returned as zero here.
asRegList :: (Word32, Word32) -> Property -> Maybe [(Word64, Word64)]
asRegList (addrWords, sizeWords) (Property _ dta) = do
  addrParser <- wordParser addrWords
  sizeParser <- wordParser sizeWords
  (_, _, v) <- rightToMaybe $ runGetOrFail (getList ((,) <$> addrParser <*> sizeParser)) (BL.fromStrict dta)
  return v
  where
    wordParser :: Word32 -> Maybe (Get Word64)
    wordParser words = case words of
      0 -> Just $ return 0
      1 -> Just $ fromIntegral <$> getWord32be
      2 -> Just $ getWord64be
      _ -> Nothing
