module Text.ODT.Diagnostics.File (saveNewODTDiag, appendToODTDiag)

where

import Text.ODT.ODT
import Text.ODT.Extract
import Text.ODT.Compress
import Text.ODT.File
import Text.ODT.Diagnostics.Utils

import Control.Monad.Writer

saveNewODTDiag :: Folderpath -> Filename -> Writer ODT () -> IO ()
saveNewODTDiag fp fn odt = do
    archive <- archiveFromZip templatesPath "empty" "./.working"

    let contentODT = getContentDocODT archive <> execWriter odt

    let archive' = replaceContentDocODT contentODT archive 
    let options = defaultODTFileOptions { workingFolder = Just "./.working", removeWorkingFolder = False, removeWorkingPath = False } 
    updateODTFile archive' templatesPath "empty" fp fn options
    prettifyODT "./.working" fn

appendToODTDiag :: Folderpath -> Filename -> Writer ODT () -> IO ()
appendToODTDiag fp fn odt = do
    archive <- archiveFromZip fp fn "./.working"

    let contentODT = getContentDocODT archive <> execWriter odt

    let archive' = replaceContentDocODT contentODT archive 
    let options = defaultODTFileOptions { workingFolder = Just "./.working", removeWorkingFolder = False, removeWorkingPath = False } 
    updateODTFile archive' fp fn fp (fn <> "_modified") options
    prettifyODT "./.working" fn