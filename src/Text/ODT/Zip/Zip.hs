module Text.ODT.Zip.Zip
    ( Text.ODT.Zip.Zip.unzip
    , Text.ODT.Zip.Zip.unzipOdt
    , Text.ODT.Zip.Zip.zipWordDir
    , Text.ODT.Zip.Zip.zipODTDir
    , Text.ODT.Zip.Zip.zipODT
    ) where

import Prelude hiding (unzip)

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

-- Zip up a folder containing an .docx archive (fpIn)
-- Write zip file to `fpOut`
zipWordDir :: Folderpath -> Filepath -> IO ()
zipWordDir fpIn fpOut = do
    archive1 <- Z.addFilesToArchive [Z.OptLocation "" False] Z.emptyArchive ["data/test/[Content_Types].xml"] 
    archive2 <- Z.addFilesToArchive [Z.OptRecursive, Z.OptLocation "_rels/" False] archive1 ["data/test/_rels/"] 
    archive3 <- Z.addFilesToArchive [Z.OptRecursive, Z.OptLocation "word/_rels/" False] archive2 ["data/test/word/_rels/"] 
    archive4 <- Z.addFilesToArchive [Z.OptLocation "word/" False] archive3 =<< (appendDirs "data/test/word/" $ listDirectory "data/test/word/")
    archive5 <- Z.addFilesToArchive [Z.OptRecursive, Z.OptLocation "docProps/" False] archive4 ["data/test/docProps/"] 
    let bytestring = Z.fromArchive archive5
    B.writeFile fpOut bytestring

-- Zip up a folder containing an .odt archive (fpIn)
-- Write zip file to `fpOut`
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

-- Unzip fpIn to fpOut
unzip :: Filepath -> Folderpath -> IO ()
unzip fpIn fpOut = do
    archive <- Z.toArchive <$> B.readFile fpIn
    Z.extractFilesFromArchive [Z.OptDestination fpOut] archive

unzipOdt :: String -> String -> String -> IO ()
unzipOdt srcpath filename dstpath = 
    unzip (srcpath <> "/" <> filename <> ".odt") (dstpath <> "/" <> filename)
