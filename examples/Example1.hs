{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Text.ODT

main :: IO ()
main = do
    archive <- archiveFromZip "../doctools-data" "example2" "../doctools-data"
    let contentODT = getContentDocODT archive <> textspan underline " Some underlined text." <> textspan italic " And some italic text"
    let archive' = replaceContentDocODT contentODT archive
    let options = defaultODTFileOptions { workingFolder = Just "../doctools-data", removeWorkingFolder = False, removeWorkingPath = False } 
    updateODTFile archive' "../doctools-data" "example2" "../doctools-data" "modified" options