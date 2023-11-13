{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.ODT.Query (
      getFirstPara
    , getLastPara
    , getParaStyleNames
    , getParas
    , getTextStyleNames
    , getText
    , getSpans
    , paraCount
    , spanCount ) where

import Text.ODT.ODT
import Text.ODT.ODTXML.ODTXML
import Text.ODT.ODTXML.Name
import Text.ODT.ODTXML.Namespace
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.List.Extra as Le


getFirstPara :: HasODT a => a -> ODT
getFirstPara x = case L.uncons . getParas . getODT $ x of
    Nothing -> EmptyODT
    Just (x, _) -> x

getLastPara :: HasODT a => a -> ODT
getLastPara x = case Le.unsnoc . getParas . getODT $ x of
    Nothing -> EmptyODT
    Just (_, x) -> x

getParas :: HasODT a => a -> [ODT]
getParas x
    | ODTSeq odt1 odt2 <- odt = getParas odt1 <> getParas odt2
    | TextNode (P pstyle) odtxml children <- odt = [TextNode (P pstyle) odtxml children]
    | TextNode _ _ _ <- odt = []
    | OfficeNode _ _ children <- odt = getParas children
    | otherwise = []
    where odt = getODT x

getText :: HasODT a => a -> T.Text
getText x
    | ODTSeq odt1 odt2 <- odt = getText odt1 <> getText odt2
    | TextLeaf typ (ODTXMLText txt) <- odt = txt
    | TextNode _ _ children <- odt = getText children
    | OfficeNode _ _ children <- odt = getText children
    | otherwise = ""
    where odt = getODT x

getSpans :: HasODT a => a -> [ODT]
getSpans x
    | ODTSeq odt1 odt2 <- odt = getSpans odt1 <> getSpans odt2
    | TextNode (Span tstyle) odtxml children <- odt = [TextNode (Span tstyle) odtxml children]
    | TextNode _ _ children <- odt = getSpans children
    | OfficeNode _ _ children <- odt = getSpans children
    | otherwise = []
    where odt = getODT x

-- Returns the names as stated on text elements
-- of the styles used

-- Returns a list of the names of the styles 
-- listed on the Paragraph elements
-- NB different from getTextStyleName, which is a 
-- function on ParaStyle
getParaStyleNames :: HasODT a => a -> [T.Text]
getParaStyleNames hasodt = getAttrVal stylename <$> (getParas . getODT $ hasodt)
    where stylename = toName TextNS "style-name"

-- Returns a list of the names of the styles 
-- listed on the Span elements
-- NB different from getTextStyleName, which is a 
-- function on TextStyle
getTextStyleNames :: HasODT a => a -> [T.Text]
getTextStyleNames hasodt = getAttrVal stylename <$> (getSpans . getODT $ hasodt)
    where stylename = toName TextNS "style-name"

paraCount :: HasODT a => a -> Int
paraCount = length . getParas . getODT

spanCount :: HasODT a => a -> Int
spanCount = length . getSpans . getODT