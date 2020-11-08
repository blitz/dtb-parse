{-# LANGUAGE TemplateHaskell #-}
module ParseSpec (spec) where

import qualified Data.ByteString   as B
import           Data.FileEmbed    (embedFile)
import           Data.Maybe        (fromJust, isJust)
import           Test.Hspec

import           Data.Dtb.LowLevel

-- |A flattened device tree describing the ULX3S running SaxonSoc.
--
-- See https://github.com/lawrie/saxonsoc-ulx3s-bin
dtbFile :: B.ByteString
dtbFile = $(embedFile "test/ulx3s-green85f.dtb")

spec :: Spec
spec = describe "low-level parser" $ do
  it "recognizes a valid header" $
    parseHeader dtbFile `shouldSatisfy` isJust
  it "recognizes a valid strings block" $
    stringsBlock (fromJust $ parseHeader dtbFile) dtbFile `shouldSatisfy` isJust
  it "recognizes a valid struct block" $
    structBlock (fromJust $ parseHeader dtbFile) dtbFile `shouldSatisfy` isJust
  it "parses memory reservations" $
    memoryReservations (fromJust $ parseHeader dtbFile) dtbFile `shouldBe` Just []
