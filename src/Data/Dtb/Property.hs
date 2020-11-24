module Data.Dtb.Property
  (Property(..), asText)
where

import qualified Data.Dtb.LowLevel        as D
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as E
import qualified Data.Text.Encoding.Error as E

-- |A property in a device tree.
data Property = Property !T.Text !D.PropData
  deriving (Show, Eq)

propData :: Property -> D.PropData
propData (Property _ dta) = dta

-- |Interpret property data as text.
asText :: Property -> T.Text
asText = E.decodeUtf8With E.lenientDecode . propData

