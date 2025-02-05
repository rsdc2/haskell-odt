{-# LANGUAGE OverloadedStrings #-}

module Text.ODT.File ( 
      concatPath
    , concatFileExt
    , appendToODTWithExtraction
    , saveNewODT
    , Filename
    , Folderpath
    , templatesPath
    , workingFolderPath ) where

import Data.List ( intercalate )
import Control.Monad.Writer

import Text.ODT.Archive
import Text.ODT.ODT
import Text.ODT.Extract
import Text.ODT.Compress

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
saveNewODTWithExtraction :: Folderpath -> Filename -> Writer ODT () -> IO ()
saveNewODTWithExtraction fp fn odt = do
    archive <- extractAndLoadArchiveFromZip templatesPath "empty" workingFolderPath
    let archive' = appendODT (execWriter odt) archive
    let options = defaultODTFileOptions { workingFolder = Just workingFolderPath, removeWorkingFolder = False, removeWorkingPath = True } 
    updateODTFileWithExtraction archive' templatesPath "empty" fp fn options

-- Save ODT content to an empty ODT file without extracting the empty ODT first
saveNewODT :: Folderpath -> Filename -> Writer ODT () -> IO ()
saveNewODT fp fn odt = do
    archive <- loadArchiveFromZip templatesPath "empty.odt"
    let archive' = appendODT (execWriter odt) archive
    updateODTFile archive' (templatesPath <> "/" <> "empty.odt") (fp <> "/" <> fn)
    putStrLn ("Written output to " <> fp <> "/" <> fn)


-- saveNewODTWithStylesDiag' :: Folderpath -> Filename -> Writer ODT () -> Writer [ParaStyle] () -> Writer [TextStyle] () -> IO ()
-- saveNewODTWithStylesDiag' fp fn odt paraStyles textStyles = do
--     archive <- extractAndLoadArchiveFromZip templatesPath "empty" workingFolderPath
--     let archive' = appendStyleODT (mconcat $ toODT <$> execWriter paraStyles) archive
--     let archive'' = appendStyleODT (mconcat $ toODT <$> execWriter textStyles) archive'
--     let archive''' = appendODT (execWriter odt) archive''
--     let options = defaultODTFileOptions { workingFolder = Just workingFolderPath, removeWorkingFolder = False, removeWorkingPath = False } 
--     updateODTFileWithExtraction archive''' templatesPath "empty" fp fn options
--     prettifyODT workingFolderPath "empty"

appendToODTWithExtraction :: Folderpath -> Filename -> Writer ODT () -> IO ()
appendToODTWithExtraction fp fn odt = do
    archive <- extractAndLoadArchiveFromZip fp fn workingFolderPath
    let archive' = appendODT (execWriter odt) archive
    let options = defaultODTFileOptions { workingFolder = Just workingFolderPath, removeWorkingFolder = True, removeWorkingPath = True } 
    updateODTFileWithExtraction archive' fp fn fp (fn <> "_modified") options