module Text.ODT.Zip.Zip
    ( Text.ODT.Zip.Zip.unzip
    , Text.ODT.Zip.Zip.zipWordDir
    , Text.ODT.Zip.Zip.zipODTDir
    , Text.ODT.Zip.Zip.zipODT
    ) where

import qualified Codec.Archive.Zip as Z
import qualified Data.ByteString.Lazy as B
import System.Directory (getDirectoryContents, listDirectory)
type Filepath = String
type Folderpath = String


appendDir :: FilePath -> FilePath -> FilePath
appendDir dir f = dir <> f

appendDirs :: FilePath -> IO [FilePath] -> IO [FilePath]
appendDirs dir fpios = do
    fps <- fpios
    let appended = appendDir dir <$> fps
    return appended

zipWordDir :: Folderpath -> Filepath -> IO ()
zipWordDir fpIn fpOut = do
    archive <- Z.addFilesToArchive [Z.OptLocation "" False] Z.emptyArchive ["data/test/[Content_Types].xml"] 
    archive <- Z.addFilesToArchive [Z.OptRecursive, Z.OptLocation "_rels/" False] archive ["data/test/_rels/"] 
    archive <- Z.addFilesToArchive [Z.OptRecursive, Z.OptLocation "word/_rels/" False] archive ["data/test/word/_rels/"] 
    archive <- Z.addFilesToArchive [Z.OptLocation "word/" False] archive =<< (appendDirs "data/test/word/" $ listDirectory "data/test/word/")
    archive <- Z.addFilesToArchive [Z.OptRecursive, Z.OptLocation "docProps/" False] archive ["data/test/docProps/"] 
    let bytestring = Z.fromArchive archive
    B.writeFile fpOut bytestring

zipODTDir :: Folderpath -> Filepath -> IO ()
zipODTDir fpIn fpOut = do
    archive <- Z.addFilesToArchive [Z.OptRecursive, Z.OptVerbose, Z.OptLocation "" False] Z.emptyArchive [fpIn] 
    -- archive <- Z.addFilesToArchive [Z.OptRecursive, Z.OptLocation "Configurations2/" False] archive [fpIn <> "Configurations2/"] 
    -- archive <- Z.addFilesToArchive [Z.OptRecursive, Z.OptLocation "word/_rels/" False] archive [fpIn"data/test/word/_rels/"] 
    -- archive <- Z.addFilesToArchive [Z.OptLocation "word/" False] archive =<< (appendDirs "data/test/word/" $ listDirectory "data/test/word/")
    -- archive <- Z.addFilesToArchive [Z.OptRecursive, Z.OptLocation "docProps/" False] archive ["data/test/docProps/"] 
    let bytestring = Z.fromArchive archive
    B.writeFile fpOut bytestring

zipODT :: Filepath -> [Filepath] -> Filepath -> IO ()
zipODT fpIn contentfps fpOut = do
    archive <- Z.toArchive <$> B.readFile fpIn
    newArchive <- Z.addFilesToArchive [Z.OptRecursive, Z.OptVerbose, Z.OptLocation "" False] archive contentfps
    let bytestring = Z.fromArchive newArchive
    B.writeFile fpOut bytestring


unzip :: Filepath -> Folderpath -> IO ()
unzip fpIn fpOut = do
    archive <- Z.toArchive <$> B.readFile fpIn
    Z.extractFilesFromArchive [Z.OptDestination fpOut] archive
