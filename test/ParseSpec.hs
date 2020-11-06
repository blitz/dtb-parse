{-# LANGUAGE TemplateHaskell #-}
module ParseSpec (spec) where

import qualified Data.ByteString as B
import           Data.FileEmbed  (embedFile)
import           Test.Hspec

import           Data.Dtb

-- |A flattened device tree describing the ULX3S running SaxonSoc.
--
-- See https://github.com/lawrie/saxonsoc-ulx3s-bin
dtbFile :: B.ByteString
dtbFile = $(embedFile "test/ulx3s-green85f.dtb")

spec :: Spec
spec = describe "parser" $ do
  it "has no tests yet" $
    True `shouldBe` True
