{-# LANGUAGE OverloadedStrings #-}

module Text.ODT.File ( 
      concatPath
    , concatFileExt
    , appendToODT
    , saveNewODT
    , Filename
    , Folderpath
    , templatesPath ) where

import Data.List ( intercalate )
import Control.Monad.Writer

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

concatFileExt :: Filename -> Ext -> Filepath
concatFileExt fn ext = fn <> "." <> ext

concatPath :: Rootpath -> [Folderpath] -> Filename -> Ext -> Filepath
concatPath fp1 fps fn ext = intercalate "/" ([fp1] <> fps) <> "/" <> fn <> "." <> ext

saveNewODT :: Folderpath -> Filename -> Writer ODT () -> IO ()
saveNewODT fp fn odt = do
    archive <- archiveFromZip templatesPath "empty" "./.working"

    let contentODT = getContentDocODT archive <> execWriter odt

    let archive' = replaceContentDocODT contentODT archive 
    let options = defaultODTFileOptions { workingFolder = Just "./.working", removeWorkingFolder = True, removeWorkingPath = True } 
    updateODTFile archive' templatesPath "empty" fp fn options

appendToODT :: Folderpath -> Filename -> Writer ODT () -> IO ()
appendToODT fp fn odt = do
    archive <- archiveFromZip fp fn "./.working"

    let contentODT = getContentDocODT archive <> execWriter odt

    let archive' = replaceContentDocODT contentODT archive 
    let options = defaultODTFileOptions { workingFolder = Just "./.working", removeWorkingFolder = True, removeWorkingPath = True } 
    updateODTFile archive' fp fn fp (fn <> "_modified") options