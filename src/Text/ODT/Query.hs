{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.ODT.Query where

import Text.ODT.ODT

getParas :: ODT -> [ODT]
getParas odt 
    | ODTSeq odt1 odt2 <- odt = getParas odt1 <> getParas odt2
    | TextNode (P pstyle) odtxml children <- odt = [TextNode (P pstyle) odtxml children]
    | TextNode _ _ _ <- odt = []
    | OfficeNode typ odtxml children <- odt = getParas children
    | otherwise = []