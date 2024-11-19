{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.ODT.Query (
      getFirstODT 
    , getFirstPara
    , getLastPara
    , getParaStyleNamesFromParaNodes
    , getParaStylesWithName
    , getParas
    , getTextStyleNamesFromParaNodes
    , getText
    , getSpans
    , paraCount
    , spanCount ) where

import Text.ODT.ODT
import Text.ODT.ODTXML.ODTXML
import Text.ODT.ODTXML.Name
import Text.ODT.ODTXML.Namespace
import Text.ODT.Style
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.List.Extra as Extra

getFirstODT :: HasODT a => a -> ODT
getFirstODT hasodt
    | ODTSeq odt1 odt2 <- odt = odt1
    | otherwise = odt
    where odt = getODT hasodt 

getFirstPara :: HasODT a => a -> ODT
getFirstPara x = case L.uncons . getParas . getODT $ x of
    Nothing -> EmptyODT
    Just (x, _) -> x

getLastPara :: HasODT a => a -> ODT
getLastPara x = case Extra.unsnoc . getParas . getODT $ x of
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

--------------------------------------------------
-- Returns the names as stated on text elements --
-- of the styles used                           --
--------------------------------------------------

-- getParaStyleNamesFromParaStyleNodes :: HasODT a => a -> [T.Text]
-- getParaStyleNamesFromParaStyleNodes hasodt = getAttrVal stylename <$> (getParaStyles . getODT $ hasodt)
--     where stylename = toName StyleNS "name"


getParaStylesWithName :: HasODT a => T.Text -> a -> [ParaStyle]
getParaStylesWithName name hasodt = filter (\x -> paraStyleName x == Just name) (getParaStyles . getODT $ hasodt)

-- Returns a list of the names of the styles 
-- listed on the Paragraph elements
-- NB different from getTextStyleName, which is a 
-- function on ParaStyle
getParaStyleNamesFromParaNodes :: HasODT a => a -> [T.Text]
getParaStyleNamesFromParaNodes hasodt = getAttrVal stylename <$> (getParas . getODT $ hasodt)
    where stylename = toName TextNS "style-name"

-- Returns a list of the names of the styles 
-- listed on the Span elements
-- NB different from getTextStyleName, which is a 
-- function on TextStyle
getTextStyleNamesFromParaNodes :: HasODT a => a -> [T.Text]
getTextStyleNamesFromParaNodes hasodt = getAttrVal stylename <$> (getSpans . getODT $ hasodt)
    where stylename = toName TextNS "style-name"

-- Counts the number of paragraphs in the HasODT item
paraCount :: HasODT a => a -> Int
paraCount = length . getParas . getODT

-- Counts the number of spans in the HasODT item
spanCount :: HasODT a => a -> Int
spanCount = length . getSpans . getODT