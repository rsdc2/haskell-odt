module Text.ODT.Diagnostics.File (
      saveNewODTDiag
    , appendToODTDiag
    , saveNewStylesDiag
    , saveNewODTWithStylesDiag
    , saveNewODTWithStylesDiag' )

where

import Text.ODT.Archive
import Text.ODT.ODT
import Text.ODT.Style.Types
import Text.ODT.Extract
import Text.ODT.Compress
import Text.ODT.File
import Text.ODT.Diagnostics.Utils

import Control.Monad.Writer

saveNewStylesDiag :: (IsStyle a, IsODT a) => Folderpath -> Filename -> Writer a () -> IO ()
saveNewStylesDiag fp fn styles = do
    archive <- archiveFromZip templatesPath "empty" workingFolderPath
    let archive' = appendStyle (execWriter styles) archive
    let options = defaultODTFileOptions { workingFolder = Just workingFolderPath, removeWorkingFolder = False, removeWorkingPath = False } 
    updateODTFile archive' templatesPath "empty" fp fn options
    prettifyODT workingFolderPath "empty"

saveNewODTWithStylesDiag :: Folderpath -> Filename -> Writer ODT () -> Writer ODT () -> IO ()
saveNewODTWithStylesDiag fp fn odt styles = do
    archive <- archiveFromZip templatesPath "empty" workingFolderPath
    let archive' = appendStyleODT (execWriter styles) archive
    let archive'' = appendODT (execWriter odt) archive'
    let options = defaultODTFileOptions { workingFolder = Just workingFolderPath, removeWorkingFolder = False, removeWorkingPath = False } 
    updateODTFile archive'' templatesPath "empty" fp fn options
    prettifyODT workingFolderPath "empty"

-- saveNewODTWithStylesDiag' :: (IsStyle a, IsODT a) => Folderpath -> Filename -> Writer ODT () -> Writer [a] () -> IO ()
saveNewODTWithStylesDiag' :: Folderpath -> Filename -> Writer ODT () -> Writer [ParaStyle] () -> Writer [TextStyle] () -> IO ()
saveNewODTWithStylesDiag' fp fn odt paraStyles textStyles = do
    archive <- archiveFromZip templatesPath "empty" workingFolderPath
    let archive' = appendStyleODT (mconcat $ toODT <$> execWriter paraStyles) archive
    let archive'' = appendStyleODT (mconcat $ toODT <$> execWriter textStyles) archive'
    let archive''' = appendODT (execWriter odt) archive''
    let options = defaultODTFileOptions { workingFolder = Just workingFolderPath, removeWorkingFolder = False, removeWorkingPath = False } 
    updateODTFile archive''' templatesPath "empty" fp fn options
    prettifyODT workingFolderPath "empty"

saveNewODTDiag :: Folderpath -> Filename -> Writer ODT () -> IO ()
saveNewODTDiag fp fn odt = do
    archive <- archiveFromZip templatesPath "empty" workingFolderPath
    let archive' = appendODT (execWriter odt) archive
    let options = defaultODTFileOptions { workingFolder = Just workingFolderPath, removeWorkingFolder = False, removeWorkingPath = False } 
    updateODTFile archive' templatesPath "empty" fp fn options
    prettifyODT workingFolderPath "empty"

appendToODTDiag :: Folderpath -> Filename -> Writer ODT () -> IO ()
appendToODTDiag fp fn odt = do
    archive <- archiveFromZip fp fn workingFolderPath
    let contentODT = getContentDocODT archive <> execWriter odt
    let archive' = replaceContentDocODT contentODT archive 
    let options = defaultODTFileOptions { workingFolder = Just workingFolderPath, removeWorkingFolder = False, removeWorkingPath = False } 
    updateODTFile archive' fp fn fp (fn <> "_modified") options
    prettifyODT workingFolderPath fn