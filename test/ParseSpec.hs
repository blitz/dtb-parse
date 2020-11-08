{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module ParseSpec (spec) where

import qualified Data.ByteString   as B
import           Data.FileEmbed    (embedFile)
import           Data.Maybe        (fromJust, isJust)
import           Test.Hspec

import           Data.Dtb.LowLevel

-- |A flattened device tree describing the ULX3S running SaxonSoc.
--
-- See https://github.com/lawrie/saxonsoc-ulx3s-bin
ulx3sDtbFile :: B.ByteString
ulx3sDtbFile = $(embedFile "test/ulx3s-green85f.dtb")

-- |A flattened device tree describing a Raspberry Pi 4B.
--
-- See https://github.com/raspberrypi/firmware
rpi4bDtbFile :: B.ByteString
rpi4bDtbFile = $(embedFile "test/rpi4b.dtb")

spec :: Spec
spec = describe "low-level parser" $ do
  it "recognizes a valid header" $
    parseHeader ulx3sDtbFile `shouldSatisfy` isJust
  it "recognizes a valid strings block" $
    stringsBlock (fromJust $ parseHeader ulx3sDtbFile) ulx3sDtbFile `shouldSatisfy` isJust
  it "recognizes a valid struct block" $
    structBlock (fromJust $ parseHeader ulx3sDtbFile) ulx3sDtbFile `shouldSatisfy` isJust
  it "parses memory reservations (ULX3S)" $
    memoryReservations (fromJust $ parseHeader ulx3sDtbFile) ulx3sDtbFile `shouldBe` Just []
  it "parses memory reservations (RPI4B)" $
    memoryReservations (fromJust $ parseHeader rpi4bDtbFile) rpi4bDtbFile `shouldBe` Just [MemoryReservation 0 0x1000]
  it "extracts strings" $
    extractString sb <$> [0, 13, 14] `shouldBe` [Just "#address-cells", Just "s", Just ""]
  it "rejects invalid offsets in the strings block" $
    extractString sb 0x1234 `shouldBe` Nothing
  it "rejects invalid UTF-8 in the strings block" $
    extractString (B.pack [0xc3, 0x28, 0]) 0 `shouldBe` Nothing
  where sb = fromJust $ stringsBlock (fromJust $ parseHeader ulx3sDtbFile) ulx3sDtbFile
