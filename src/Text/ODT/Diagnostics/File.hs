module Text.ODT.Diagnostics.File (
      writeNewODTDiag
    , appendToODTDiag
    , writeNewStylesDiag
    , writeNewODTWithStylesDiag
    , writeNewODTWithStylesDiag' )

where

import Text.ODT.Archive
import Text.ODT.ODT
import Text.ODT.Style.Types
import Text.ODT.Extract
import Text.ODT.Compress
import Text.ODT.File
import Text.ODT.Diagnostics.Utils

import Control.Monad.Writer

writeNewStylesDiag :: (IsStyle a, IsODT a) => Folderpath -> Filename -> Writer a () -> IO ()
writeNewStylesDiag fp fn styles = do
    archive <- extractAndLoadArchiveFromZip templatesPath "empty" workingFolderPath
    let archive' = appendStyle (execWriter styles) archive
    let options = defaultODTFileOptions { workingFolder = Just workingFolderPath, removeWorkingFolder = False, removeWorkingPath = False } 
    updateODTFileWithExtraction archive' templatesPath "empty" fp fn options
    prettifyODT workingFolderPath "empty"

writeNewODTWithStylesDiag :: Folderpath -> Filename -> Writer ODT () -> Writer ODT () -> IO ()
writeNewODTWithStylesDiag fp fn odt styles = do
    archive <- extractAndLoadArchiveFromZip templatesPath "empty" workingFolderPath
    let archive' = appendStyleODT (execWriter styles) archive
    let archive'' = appendODT (execWriter odt) archive'
    let options = defaultODTFileOptions { workingFolder = Just workingFolderPath, removeWorkingFolder = False, removeWorkingPath = False } 
    updateODTFileWithExtraction archive'' templatesPath "empty" fp fn options
    prettifyODT workingFolderPath "empty"

writeNewODTWithStylesDiag' :: Folderpath -> Filename -> Writer ODT () -> Writer [ParaStyle] () -> Writer [TextStyle] () -> IO ()
writeNewODTWithStylesDiag' fp fn odt paraStyles textStyles = do
    archive <- extractAndLoadArchiveFromZip templatesPath "empty" workingFolderPath
    let archive' = appendStyleODT (mconcat $ toODT <$> execWriter paraStyles) archive
    let archive'' = appendStyleODT (mconcat $ toODT <$> execWriter textStyles) archive'
    let archive''' = appendODT (execWriter odt) archive''
    let options = defaultODTFileOptions { workingFolder = Just workingFolderPath, removeWorkingFolder = False, removeWorkingPath = False } 
    updateODTFileWithExtraction archive''' templatesPath "empty" fp fn options
    prettifyODT workingFolderPath "empty"

writeNewODTDiag :: Folderpath -> Filename -> Writer ODT () -> IO ()
writeNewODTDiag fp fn odt = do
    archive <- extractAndLoadArchiveFromZip templatesPath "empty" workingFolderPath
    let archive' = appendODT (execWriter odt) archive
    let options = defaultODTFileOptions { workingFolder = Just workingFolderPath, removeWorkingFolder = False, removeWorkingPath = False } 
    updateODTFileWithExtraction archive' templatesPath "empty" fp fn options
    prettifyODT workingFolderPath "empty"

appendToODTDiag :: Folderpath -> Filename -> Writer ODT () -> IO ()
appendToODTDiag fp fn odt = do
    archive <- extractAndLoadArchiveFromZip fp fn workingFolderPath
    let contentODT = getContentDocODT archive <> execWriter odt
    let archive' = replaceContentDocODT contentODT archive 
    let options = defaultODTFileOptions { workingFolder = Just workingFolderPath, removeWorkingFolder = False, removeWorkingPath = False } 
    updateODTFileWithExtraction archive' fp fn fp (fn <> "_modified") options
    prettifyODT workingFolderPath fn