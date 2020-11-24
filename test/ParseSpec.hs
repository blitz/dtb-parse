{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module ParseSpec (spec) where

import qualified Data.ByteString   as B
import           Data.FileEmbed    (embedFile)
import           Data.Functor
import           Data.Maybe        (fromJust, isJust)
import           Test.Hspec

import           Data.Dtb
import           Data.Dtb.LowLevel
import           Data.Dtb.Parser

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
  it "tokenizes a token stream (1)" $
    fromJust (deviceTreeTokens sb stb) `shouldStartWith` [BeginNode ""]
  it "tokenizes a token stream (2)" $
    fromJust (deviceTreeTokens sb stb) `shouldEndWith` [End]
  it "parses a token stream" $
    fromJust (parse (fromJust $ deviceTreeTokens sb stb)) `shouldSatisfy` isSaneDeviceTree

  it "looks up the root node" $
    lookupNode "/" <$> parseDtb ulx3sDtbFile `shouldBe` Just <$> rootNode <$> parseDtb ulx3sDtbFile
  it "doesn't lookup non-existing paths" $
    lookupNode "/cpus/cxxpu@0" <$> parseDtb ulx3sDtbFile `shouldBe` Just Nothing
  it "looks up existing paths without aliases" $
    lookupNode "/cpus/cpu@0" <$> parseDtb ulx3sDtbFile `shouldSatisfy` isJust . fromJust

  it "looks up existing paths with aliases" $
    lookupNode "/ethernet0" <$> parseDtb rpi4bDtbFile `shouldBe` lookupNode "/scb/ethernet@7d580000" <$> parseDtb rpi4bDtbFile
  -- TODO Not implemented yet.
  -- it "looks up paths without unit addresses in unambiguous cases" $
  --   lookupNode "/scb/ethernet" <$> parseDtb rpi4bDtbFile `shouldBe` lookupNode "/scb/ethernet@7d580000" <$> parseDtb rpi4bDtbFile

  it "looks up string properties" $
    ((parseDtb rpi4bDtbFile
      >>= lookupNode "/scb/ethernet@7d580000"
      >>= lookupProperty "phy-mode") <&> asText) `shouldBe` Just "rgmii-rxid"

  where sb = fromJust $ stringsBlock (fromJust $ parseHeader ulx3sDtbFile) ulx3sDtbFile
        stb = fromJust $ structBlock (fromJust $ parseHeader ulx3sDtbFile) ulx3sDtbFile
        isSaneDeviceTree (Node "" props children) = True
        isSaneDeviceTree _                        = False
