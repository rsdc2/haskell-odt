{-# LANGUAGE OverloadedStrings #-}

module Text.ODT.File ( 
      appendToODTWithExtraction
    , concatPath
    , concatFileExt
    , Filename
    , Folderpath
    , writeNewODT
    , writeNewODTWithStyles
    , templatesPath
    , workingFolderPath ) where

import Data.List ( intercalate )
import Control.Monad.Writer

import Text.ODT.Archive
import Text.ODT.ODT
import Text.ODT.Extract
import Text.ODT.Compress
import Text.ODT.Style.Types

type Filename = String
type Rootpath = String
type Filepath = String 
type Folderpath = String
type Ext = String

templatesPath :: Folderpath
templatesPath = "./templates"

workingFolderPath :: Folderpath
workingFolderPath = "./.odt-work"

concatFileExt :: Filename -> Ext -> Filepath
concatFileExt fn ext = fn <> "." <> ext

concatPath :: Rootpath -> [Folderpath] -> Filename -> Ext -> Filepath
concatPath fp1 fps fn ext = intercalate "/" ([fp1] <> fps) <> "/" <> fn <> "." <> ext

-- Save ODT content to an empty ODT file by extracting the empty ODT first
writeNewODTWithExtraction :: Folderpath -> Filename -> Writer ODT () -> IO ()
writeNewODTWithExtraction fp fn odt = do
    archive <- extractAndLoadArchiveFromZip templatesPath "empty" workingFolderPath
    let archive' = appendODT (execWriter odt) archive
    let options = defaultODTFileOptions { workingFolder = Just workingFolderPath, removeWorkingFolder = False, removeWorkingPath = True } 
    updateODTFileWithExtraction archive' templatesPath "empty" fp fn options

-- Save ODT content to an empty ODT file without extracting the empty ODT first
writeNewODT :: Folderpath -> Filename -> Writer ODT () -> IO ()
writeNewODT fp fn odt = do
    archive <- loadArchiveFromZip templatesPath "empty.odt"
    let archive' = appendODT (execWriter odt) archive
    updateODTFile archive' (templatesPath <> "/" <> "empty.odt") (fp <> "/" <> fn)
    putStrLn ("Written output to " <> fp <> "/" <> fn)

writeNewODTWithStyles :: Folderpath -> Filename -> Writer [ParaStyle] () -> Writer [TextStyle] () -> Writer ODT () -> IO ()
writeNewODTWithStyles fp fn paraStyles textStyles odt = do
    archive <- loadArchiveFromZip templatesPath "empty.odt"
    let archiveWithParaStyles = appendStyleODT (mconcat $ toODT <$> execWriter paraStyles) archive
    let archiveWithStyles = appendStyleODT (mconcat $ toODT <$> execWriter textStyles) archiveWithParaStyles
    let archiveWithContent = appendODT (execWriter odt) archiveWithStyles
    updateODTFile archiveWithContent (templatesPath <> "/" <> "empty.odt") (fp <> "/" <> fn)
    putStrLn ("Written output to " <> fp <> "/" <> fn)

appendToODTWithExtraction :: Folderpath -> Filename -> Writer ODT () -> IO ()
appendToODTWithExtraction fp fn odt = do
    archive <- extractAndLoadArchiveFromZip fp fn workingFolderPath
    let archive' = appendODT (execWriter odt) archive
    let options = defaultODTFileOptions { workingFolder = Just workingFolderPath, removeWorkingFolder = True, removeWorkingPath = True } 
    updateODTFileWithExtraction archive' fp fn fp (fn <> "_modified") options