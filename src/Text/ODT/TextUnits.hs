{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.ODT.TextUnits (
    Text.ODT.TextUnits.para
  , Text.ODT.TextUnits.textspan
  , Text.ODT.TextUnits.str
  , textspanM
  , paraM
) where

import Data.Text as T
import Data.Map as Map

import Text.ODT.ODT
import Text.ODT.Style
import Text.ODT.ODTXML.ODTXML
import Text.ODT.ODTXML.Name
import Text.ODT.ODTXML.Namespace

import Control.Monad.Writer


para :: ParaStyle -> T.Text -> ODT
para parastyle s = TextNode (P $ Just parastyle) (ODTXMLElem pName Map.empty) (TextLeaf Str $ ODTXMLText s)

paraM :: ParaStyle -> T.Text -> Writer ODT ()
paraM style = tell . para style

-- Passes the TextStyle on with the TextNode, and get the name when finally integrate into the document
textspan :: TextStyle -> T.Text -> ODT
textspan textstyle s = TextNode (Span $ Just textstyle) (ODTXMLElem spanName Map.empty) (TextLeaf Str $ ODTXMLText s)

textspanM :: TextStyle -> T.Text -> Writer ODT ()
textspanM style = tell . textspan style

str :: T.Text -> ODT
str s = TextLeaf Str (ODTXMLText s)

strM :: T.Text -> Writer ODT ()
strM = tell . str


