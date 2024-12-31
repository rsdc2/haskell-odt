module Text.ODT.Zip.Zip
    ( Text.ODT.Zip.Zip.unzip
    , Text.ODT.Zip.Zip.unzipOdt
    , Text.ODT.Zip.Zip.zipWordDir
    , Text.ODT.Zip.Zip.zipODTDir
    , Text.ODT.Zip.Zip.zipODT
    ) where

import Prelude hiding (unzip)

import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy as ByteString
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

-- Zip up a folder containing an .docx archive (fpIn)
-- Write zip file to `fpOut`
zipWordDir :: Folderpath -> Filepath -> IO ()
zipWordDir fpIn fpOut = do
    archive1 <- Zip.addFilesToArchive [Zip.OptLocation "" False] Zip.emptyArchive ["data/test/[Content_Types].xml"] 
    archive2 <- Zip.addFilesToArchive [Zip.OptRecursive, Zip.OptLocation "_rels/" False] archive1 ["data/test/_rels/"] 
    archive3 <- Zip.addFilesToArchive [Zip.OptRecursive, Zip.OptLocation "word/_rels/" False] archive2 ["data/test/word/_rels/"] 
    archive4 <- Zip.addFilesToArchive [Zip.OptLocation "word/" False] archive3 =<< (appendDirs "data/test/word/" $ listDirectory "data/test/word/")
    archive5 <- Zip.addFilesToArchive [Zip.OptRecursive, Zip.OptLocation "docProps/" False] archive4 ["data/test/docProps/"] 
    let bytestring = Zip.fromArchive archive5
    ByteString.writeFile fpOut bytestring

-- Zip up a folder containing an .odt archive (fpIn)
-- Write zip file to `fpOut`
zipODTDir :: Folderpath -> Filepath -> IO ()
zipODTDir fpIn fpOut = do
    archive <- Zip.addFilesToArchive [Zip.OptRecursive, Zip.OptVerbose, Zip.OptLocation "" False] Zip.emptyArchive [fpIn] 
    let bytestring = Zip.fromArchive archive
    ByteString.writeFile fpOut bytestring

zipODT :: Filepath -> [Filepath] -> Filepath -> IO ()
zipODT fpIn contentFps fpOut = do
    archive <- Zip.toArchive <$> ByteString.readFile fpIn
    newArchive <- Zip.addFilesToArchive [Zip.OptRecursive, Zip.OptVerbose, Zip.OptLocation "" False] archive contentFps
    let bytestring = Zip.fromArchive newArchive
    ByteString.writeFile fpOut bytestring

-- Unzip fpIn to fpOut
unzip :: Filepath -> Folderpath -> IO ()
unzip fpIn fpOut = do
    archive <- Zip.toArchive <$> ByteString.readFile fpIn
    Zip.extractFilesFromArchive [Zip.OptDestination fpOut] archive

unzipOdt :: String -> String -> String -> IO ()
unzipOdt srcpath filename dstpath = 
    unzip (srcpath <> "/" <> filename <> ".odt") (dstpath <> "/" <> filename)
