{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Text.ODT.Extract ( archiveFromZip )
import Text.ODT.Compress ( updateODTFile, defaultODTFileOptions, ODTFileOptions(..) )
import Text.ODT.ODT ( HasContentODT(..) )
import qualified Text.ODT.TextUnits as TU

import qualified Text.ODT.Style.TextStyles as TS

main :: IO ()
main = do
    archive <- archiveFromZip "../doctools-data" "example2" "../doctools-data"
    let contentODT = getContentDocODT archive <> TU.span TS.underline " Some underlined text." <> TU.span TS.italic " And some italic text"
    let archive' = replaceContentDocODT contentODT archive
    let options = defaultODTFileOptions { workingFolder = Just "../doctools-data", removeWorkingFolder = False, removeWorkingPath = False } 
    updateODTFile archive' "../doctools-data" "example2" "../doctools-data" "modified" options