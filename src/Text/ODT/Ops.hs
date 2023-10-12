{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.ODT.Ops (
    Text.ODT.Ops.p
  , Text.ODT.Ops.span
  , Text.ODT.Ops.str
) where

import Data.Text as T
import Data.Map as Map

import Text.ODT.ODT
import Text.ODT.Style
import Text.ODT.ODTXML.ODTXML
import Text.ODT.ODTXML.Name
import Text.ODT.ODTXML.Namespace


p :: ParaStyle -> T.Text -> ODT
p parastyle s = TextNode (P $ Just parastyle) (ODTXMLElem pName Map.empty) (TextLeaf Str $ ODTXMLText s)

span :: TextStyle -> T.Text -> ODT
-- Passes the TextStyle on with the TextNode, and get the name when finally integrate into the document
span textstyle s = TextNode (Span $ Just textstyle) (ODTXMLElem spanName Map.empty) (TextLeaf Str odtxmltext)
    where odtxmltext = ODTXMLText s

str :: T.Text -> ODT
str s = TextLeaf Str (ODTXMLText s)
