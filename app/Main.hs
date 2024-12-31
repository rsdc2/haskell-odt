{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Text.XML as XML

import Text.ODT.Diagnostics.Utils ( prettifyODT )
import Text.ODT.Extract ( archiveFromZip )
import Text.ODT.Compress ( updateArchive )

import Text.ODT.File ( concatPath )
import qualified Text.ODT.Zip.Zip as Zip 
import Text.ODT.ODT ( HasContentODT(..) )
import qualified Text.ODT.Ops as ODT

import qualified Text.ODT.Style.TextStyles as TextStyles

main :: IO ()
main = do
    archive <- archiveFromZip "../doctools-data" "example2" "../doctools-data"
    let contentODT = getContentDocODT archive <> ODT.span TextStyles.underline " Some underlined text." <> ODT.span TextStyles.italic " And some italic text"
    let archive' = replaceContentDocODT contentODT archive
    updateArchive archive' "../doctools-data" "example2" "../doctools-data" "modified"