{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.ODT.Query where

import Text.ODT.ODT
import Text.ODT.ODTXML.ODTXML
import qualified Data.Text as T


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

paraCount :: HasODT a => a -> Int
paraCount = length . getParas . getODT

spanCount :: HasODT a => a -> Int
spanCount = length . getSpans . getODT