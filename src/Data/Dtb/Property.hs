module Data.Dtb.Property
  (Property(..), propName, propData, asText)
where

import qualified Data.ByteString          as B
import qualified Data.Dtb.LowLevel        as D
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as E
import qualified Data.Text.Encoding.Error as E

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
