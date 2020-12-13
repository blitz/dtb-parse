{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module ParseSpec (spec) where

import qualified Data.ByteString         as B
import           Data.Either             (fromRight, isLeft, isRight)
import           Data.Either.Combinators (rightToMaybe)
import           Data.FileEmbed          (embedFile)
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
    parseHeader ulx3sDtbFile `shouldSatisfy` isRight
  it "recognizes a valid strings block" $
    stringsBlock ulx3sHeader ulx3sDtbFile `shouldSatisfy` isRight
  it "recognizes a valid struct block" $
    structBlock ulx3sHeader ulx3sDtbFile `shouldSatisfy` isRight
  it "parses memory reservations (ULX3S)" $
    memoryReservations ulx3sHeader ulx3sDtbFile `shouldBe` Right []
  it "parses memory reservations (RPI4B)" $
    memoryReservations rpi4bHeader rpi4bDtbFile `shouldBe` Right [MemoryReservation 0 0x1000]
  it "extracts strings" $
    extractString sb <$> [0, 13, 14] `shouldBe` [Right "#address-cells", Right "s", Right ""]
  it "rejects invalid offsets in the strings block" $
    extractString sb 0x1234 `shouldSatisfy` isLeft
  it "rejects invalid UTF-8 in the strings block" $
    extractString (B.pack [0xc3, 0x28, 0]) 0 `shouldSatisfy` isLeft
  it "tokenizes a token stream (1)" $
    fromRight (error "tokens") (deviceTreeTokens sb stb) `shouldStartWith` [BeginNode ""]
  it "tokenizes a token stream (2)" $
    fromRight (error "tokens") (deviceTreeTokens sb stb) `shouldEndWith` [End]
  it "parses a token stream" $
    fromRight (error "parse") (parse (fromRight (error "tokens") $ deviceTreeTokens sb stb)) `shouldSatisfy` isSaneDeviceTree

  it "looks up the root node" $
    lookupNode "/" <$> parseDtb ulx3sDtbFile `shouldBe` Just <$> rootNode <$> parseDtb ulx3sDtbFile
  it "doesn't lookup non-existing paths" $
    lookupNode "/cpus/cxxpu@0" <$> parseDtb ulx3sDtbFile `shouldBe` Right Nothing
  it "looks up existing paths without aliases" $
    lookupNode "/cpus/cpu@0" <$> parseDtb ulx3sDtbFile `shouldSatisfy` isRight

  it "looks up existing paths with aliases" $
    lookupNode "/ethernet0" <$> parseDtb rpi4bDtbFile `shouldBe` lookupNode "/scb/ethernet@7d580000" <$> parseDtb rpi4bDtbFile
  it "looks up paths without unit addresses in unambiguous cases" $
    lookupNode "/scb/ethernet" <$> parseDtb rpi4bDtbFile `shouldBe` lookupNode "/scb/ethernet@7d580000" <$> parseDtb rpi4bDtbFile

  it "looks up string properties" $
    (do
         dtb <- rightToMaybe $ parseDtb rpi4bDtbFile
         node <- lookupNode "/scb/ethernet@7d580000" dtb
         prop <- lookupProperty "phy-mode" node
         return $ asText prop) `shouldBe` Just "rgmii-rxid"

  it "looks up resource lists (32-bit)" $
    (do
        addrSize <- addressSizeCells "/framebuffer" ulx3sDtb
        fb <- lookupNode "/framebuffer" ulx3sDtb
        prop <- lookupProperty "reg" fb
        asRegList addrSize prop) `shouldBe` Just [(0x80e00000, 0x96000)]

  it "looks up resource lists (64-bit)" $
    (do
        addrSize <- addressSizeCells "/v3dbus/v3d" rpi4bDtb
        fb <- lookupNode "/v3dbus/v3d" rpi4bDtb
        prop <- lookupProperty "reg" fb
        asRegList addrSize prop) `shouldBe` Just [(0x7ec00000, 0x4000),
                                                  (0x7ec04000, 0x4000)]


  where
    ulx3sHeader :: Header
    ulx3sHeader = fromRight (error "parseHeader failed") $ parseHeader ulx3sDtbFile
    ulx3sDtb = fromRight (error "parseDtb failed") $ parseDtb ulx3sDtbFile

    rpi4bHeader :: Header
    rpi4bHeader = fromRight (error "parseHeader failed") $ parseHeader rpi4bDtbFile
    rpi4bDtb = fromRight (error "parseDtb failed") $ parseDtb rpi4bDtbFile

    sb = fromRight (error "stringsBlock") $ stringsBlock ulx3sHeader ulx3sDtbFile
    stb = fromRight (error "structBlock") $ structBlock ulx3sHeader ulx3sDtbFile
    isSaneDeviceTree (Node "" props children) = True
    isSaneDeviceTree _                        = False
