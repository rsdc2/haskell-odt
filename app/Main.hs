{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Text.XML as XML

import Text.ODT.Diagnostics.Utils ( prettifyOdt )
import Text.ODT.Extract ( archiveFromZip )
import Text.ODT.Compress ( saveToArchive )

import Text.ODT.File ( concatPath )
import qualified Text.ODT.Zip.Zip as Zip 
import Text.ODT.ODT ( HasODT(getODT), HasContentODT(..))
import Text.ODT.Doc ( Doc(odt), IsXMLDoc(fromXMLDoc, toXMLDoc) )
import qualified Text.ODT.Ops as ODT

import qualified Text.ODT.Style.TextStyles as TextStyles

exampleFilename = "example2"
folderpath = "../doctools-data" 
rootPath = concatPath folderpath []
examplePath = concatPath folderpath [exampleFilename]

main :: IO ()
main = do
    archive <- archiveFromZip folderpath exampleFilename folderpath
    let contentOdt = contentODT archive <> ODT.span TextStyles.underline " Some underlined text."
    let archive' = replaceContentODT contentOdt archive
    saveToArchive archive' folderpath exampleFilename folderpath