{-# LANGUAGE OverloadedStrings #-}

module Text.ODT.File ( concatPath, concatFileExt ) where

import Data.List ( intersperse, intercalate )

type Filename = String
type Rootpath = String
type Filepath = String 
type Folderpath = String
type Ext = String


concatFileExt :: Filename -> Ext -> Filepath
concatFileExt fn ext = fn <> "." <> ext

concatPath :: Rootpath -> [Folderpath] -> Filename -> Ext -> Filepath
concatPath fp1 fps fn ext = intercalate "/" ([fp1] <> fps) <> "/" <> fn <> "." <> ext
