{-# LANGUAGE OverloadedStrings #-}
module Data.Dtb
  (Dtb(..), MemoryReservation(..), Property(..), DeviceTree(..)
  , parseDtb
  , lookupNode
  , lookupProperty
  , rootNode
  , asText
  )
where

import qualified Data.ByteString   as B
import           Data.Dtb.LowLevel
import           Data.Dtb.Parser
import           Data.Dtb.Property
import           Data.Foldable     (find)
import           Data.Maybe        (catMaybes, listToMaybe)
import qualified Data.Text         as T

-- |The representation of a device tree.
--
-- Memory reservations are not part of the tree structure itself, but
-- are represented using a list. Everything else exists as a
-- `DeviceTree` tree.
data Dtb = Dtb [MemoryReservation] DeviceTree

-- |Parses a device tree from a `ByteString`.
--
-- Returns Nothing on failure.
parseDtb :: B.ByteString -> Maybe Dtb
parseDtb dta = do
  header <- parseHeader dta
  memRsv <- memoryReservations header dta
  sb <- stringsBlock header dta
  stb <- structBlock header dta
  tokens <- deviceTreeTokens sb stb
  tree <- parse tokens
  return $ Dtb memRsv tree

-- |Returns the root node of the device tree.
rootNode :: Dtb -> DeviceTree
rootNode (Dtb _ n) = n

-- |Split a path into its components.
toPath :: T.Text -> [T.Text]
toPath p = if split == ["", ""] then [""] else split
  -- "/" is split into two empty strings. We want [""] instead.
  where split = T.splitOn "/" $ if T.isPrefixOf "/" p then p else "/" <> p

lookupNodeByPath :: [T.Text] -> DeviceTree -> Maybe DeviceTree
lookupNodeByPath [] n = Nothing
lookupNodeByPath (p:ps) n@(Node name _ children)
  | p == name = case ps of
      [] -> Just n
      _  -> listToMaybe $ catMaybes $ lookupNodeByPath ps <$> children
  | otherwise = Nothing

-- |Look up a property in the device tree using its path.
--
-- The path is a string like "/cpus/cpu@0". This function currently
-- does NOT support aliases.
lookupNode :: T.Text -> Dtb -> Maybe DeviceTree
lookupNode name (Dtb _ n) = lookupNodeByPath (toPath name) n

-- |Find a property with the given name in the property list of a
-- node.
lookupProperty :: T.Text -> DeviceTree -> Maybe Property
lookupProperty name (Node _ props _) = find ((== name) . propName) props
