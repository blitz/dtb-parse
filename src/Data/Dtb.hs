{-# LANGUAGE OverloadedStrings #-}
module Data.Dtb
  (Dtb(..), MemoryReservation(..), Property(..), DeviceTree(..)
  , parseDtb
  , matchesPath
  , lookupNode
  , lookupProperty
  , rootNode
  , reservedMemory
  , addressSizeCells
  , asText
  , asWord32
  , asWord64
  , asRegList
  )
where

import qualified Data.ByteString   as B
import           Data.Dtb.LowLevel
import           Data.Dtb.Parser
import           Data.Dtb.Property
import           Data.Foldable     (find)
import           Data.List         (isPrefixOf)
import           Data.Maybe        (catMaybes, isJust, listToMaybe)
import qualified Data.Text         as T
import           Data.Word         (Word32)

-- |The representation of a device tree.
--
-- Memory reservations are not part of the tree structure itself, but
-- are represented using a list. Everything else exists as a
-- `DeviceTree` tree.
data Dtb = Dtb [MemoryReservation] DeviceTree
  deriving (Show)

-- |Parses a device tree from a `ByteString`.
--
-- Returns an error message on failure.
parseDtb :: B.ByteString -> Either T.Text Dtb
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

reservedMemory :: Dtb -> [MemoryReservation]
reservedMemory (Dtb m _) = m

-- |A list of node names is a path through the tree.
type Path = [T.Text]

-- |Split a path into its components.
toPath :: T.Text -> Path
toPath p = if split == ["", ""] then [""] else split
  -- "/" is split into two empty strings. We want [""] instead.
  where split = T.splitOn "/" $ if T.isPrefixOf "/" p then p else "/" <> p


-- |Split a path component into name and unit address.
--
-- As the unit address is optional, it can be Nothing.
splitName :: T.Text -> (T.Text, Maybe T.Text)
splitName name = if isJust (T.find (== '@') name) then
                   let (n, u) = T.breakOn "@" name in
                     (n, Just u)
                 else
                   (name, Nothing)

-- |Check whether two names match.
matchesNames :: (T.Text, Maybe T.Text) -> (T.Text, Maybe T.Text) -> Bool
matchesNames (a, Nothing) (b, _) = a == b
matchesNames (a, _) (b, Nothing) = a == b
matchesNames a b                 = a == b

-- |Check whether a name matches a device tree node.
matchesPath :: T.Text -> DeviceTree -> Bool
matchesPath p (Node name _ _) = matchesNames splitP splitN
  where
    splitP = splitName p
    splitN = splitName name

-- |Look up a device tree node by its path.
--
-- This function does not handle aliases.
lookupNodeByPath :: Path -> DeviceTree -> Maybe DeviceTree
lookupNodeByPath [] n = Nothing
lookupNodeByPath (p:ps) n@(Node _ _ children)
  | matchesPath p n = case ps of
      [] -> Just n
      _  -> listToMaybe $ catMaybes $ lookupNodeByPath ps <$> children
  | otherwise = Nothing

-- |An alias is is a pair of alias name and path it points to.
type Alias = (T.Text, Path)

-- |The list of aliases defined in the device tree.
--
-- This list is empty if there is no alias node or the alias node is
-- empty.
aliases :: Dtb -> [Alias]
aliases dtb = map (\p -> (propName p, toPath $ asText p))
  $ case lookupNodeByPath ["", "aliases"] (rootNode dtb) of
      Just (Node _ props _) -> props
      _                     -> []

-- |Try to replace an alias in a path.
--
-- Only a single alias is resolved and only of it occurs at the start
-- of the path.
resolveAliases :: [Alias] -> Path -> Path
resolveAliases _ []           = []
resolveAliases aliases path = case find (\(a, _) -> isPrefixOf ["", a] path) aliases of
  Just (_, replacement) -> replacement ++ drop 2 path
  Nothing               -> path

-- |Convert a textual path to a path structure.
--
-- This function also resolves aliases.
resolvePath :: T.Text -> Dtb -> Path
resolvePath name dtb = resolveAliases (aliases dtb) $ toPath name

-- |Look up a property in the device tree using its path.
--
-- The path is a string like "/cpus/cpu@0". This function currently
-- does NOT support aliases.
lookupNode :: T.Text -> Dtb -> Maybe DeviceTree
lookupNode name dtb@(Dtb _ n) = lookupNodeByPath (resolvePath name dtb) n

-- |Find a property with the given name in the property list of a
-- node.
lookupProperty :: T.Text -> DeviceTree -> Maybe Property
lookupProperty name (Node _ props _) = find ((== name) . propName) props

dropLast :: [a] -> [a]
dropLast []     = error "Cannot drop last element from empty list"
dropLast (_:[]) = []
dropLast (x:xs) = x:(dropLast xs)

-- |Returns the #address-cells and #size-cells properties of a
-- node.
--
-- If the node doesn't have the required properties the tree is
-- traversed upwards to the first node that has them.
--
-- This function can be used together with `asRegList` to parse
-- resource lists.
addressSizeCells :: T.Text -> Dtb -> Maybe (Word32, Word32)
addressSizeCells name dtb@(Dtb _ n) = descend path
  where
    descend :: Path -> Maybe (Word32, Word32)
    descend [] = Nothing
    descend path = case lookupNodeByPath path n >>= addressSizeCellsExact of
      Just addrSize -> Just addrSize
      Nothing       -> descend $ dropLast path
    path = resolvePath name dtb
    addressSizeCellsExact dtb' =  (,) <$> propAsWord32 dtb' "#address-cells" <*> propAsWord32 dtb' "#size-cells"
    propAsWord32 t name = lookupProperty name t >>= asWord32
