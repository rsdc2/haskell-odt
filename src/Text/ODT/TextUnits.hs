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


para :: Maybe ParaStyle -> T.Text -> ODT
para parastyle s = TextNode (P parastyle) (ODTXMLElem pName Map.empty) (TextLeaf Str $ ODTXMLText s)

textspan :: Maybe TextStyle -> T.Text -> ODT
-- Passes the TextStyle on with the TextNode, and get the name when finally integrate into the document
textspan textstyle s = TextNode (Span textstyle) (ODTXMLElem spanName Map.empty) (TextLeaf Str $ ODTXMLText s)

str :: T.Text -> ODT
str s = TextLeaf Str (ODTXMLText s)

textspanM :: Maybe TextStyle -> T.Text -> Writer ODT ()
textspanM style s = tell $ textspan style s

paraM :: Maybe ParaStyle -> T.Text -> Writer ODT ()
paraM style s = tell $ para style s
