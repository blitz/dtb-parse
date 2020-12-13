{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Data.Dtb.Parser
  (DeviceTree(..), parse)
where

import qualified Data.Dtb.LowLevel as D
import           Data.Dtb.Property
import qualified Data.Set          as E
import qualified Data.Text         as T
import           Data.Void         (Void)
import qualified Text.Megaparsec   as P

data DeviceTree = Node !T.Text [Property] [DeviceTree]
  deriving (Show, Eq)

type Parser = P.Parsec Void [D.Token]

beginNodeName :: D.Token -> Maybe T.Text
beginNodeName (D.BeginNode n) = Just n
beginNodeName _               = Nothing

tokenToProperty :: D.Token -> Maybe Property
tokenToProperty (D.Prop n d) = Just $ Property n d
tokenToProperty _            = Nothing

propertyParser :: Parser Property
propertyParser = P.token tokenToProperty E.empty

nodeParser :: Parser DeviceTree
nodeParser = do
  name <- P.token beginNodeName E.empty
  props <- P.many propertyParser
  children <- P.many nodeParser
  P.single D.EndNode
  return $ Node name props children

deviceTreeParser :: Parser DeviceTree
deviceTreeParser = do
  rootNode <- nodeParser
  P.single D.End
  P.eof
  return rootNode

parse :: [D.Token] -> Either T.Text DeviceTree
parse ts = case P.parseMaybe deviceTreeParser ts of
             -- TODO We can use P.parse here, return Either and give a
             -- real error message, if we sort out what instances we
             -- have to implement to make megaparsec happy.
             Nothing   -> Left "failed to parse device tree structure"
             Just tree -> Right tree
