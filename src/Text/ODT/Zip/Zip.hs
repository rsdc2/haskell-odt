module Text.ODT.Zip.Zip
    ( archiveFromFile
    , archiveToZipLBS
    , replaceMimetype
    , saveFileLbsToZipArchive
    , Text.ODT.Zip.Zip.unzip
    , Text.ODT.Zip.Zip.unzipOdt
    , Text.ODT.Zip.Zip.zipODTDir
    , Text.ODT.Zip.Zip.zipODT
    , Text.ODT.Zip.Zip.fileTextFromZip
    , Text.ODT.Zip.Zip.fileLBSFromZip
    ) where

import Prelude hiding (unzip)
import qualified Data.Text as T 
import Data.Text.Encoding
import Data.Time.Clock.POSIX
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

-- Return a zip archive from a file at the specified file path
archiveFromFile :: FilePath -> IO Zip.Archive
archiveFromFile fp = Zip.toArchive <$> ByteString.readFile fp

-- Returns a LazyByteString of a file within a zip archive structure
fileLBSFromZip :: FilePath -> FilePath -> IO ByteString.ByteString
fileLBSFromZip filePath archivePath = do
    file <- ByteString.readFile filePath
    let archive = Zip.toArchive file
    let entry = Zip.findEntryByPath archivePath archive
    let decompressed = Zip.fromEntry <$> entry
    case decompressed of 
        Just bytestring     -> pure bytestring
        Nothing             -> pure ByteString.empty 

archiveToZipLBS :: Zip.Archive -> ByteString.ByteString
archiveToZipLBS = Zip.fromArchive

-- Return a LazyByteString as a Zip archive entry
fileLbsToEntry :: ByteString.ByteString -> FilePath -> IO Zip.Entry
fileLbsToEntry lbs fp = do
    time <- round <$> getPOSIXTime
    pure $ Zip.toEntry fp time lbs


fileTextFromZip :: FilePath -> FilePath -> IO T.Text
fileTextFromZip filePath archivePath = do
    bytestring <- fileLBSFromZip filePath archivePath
    pure . decodeUtf8Lenient . ByteString.toStrict $ bytestring


-- Save a file LazyByteString to a zip archive with a specified filepath within the archive
saveFileLbsToZipArchive :: ByteString.ByteString -> Filepath -> Zip.Archive -> IO Zip.Archive
saveFileLbsToZipArchive lbs fp archive = do
    entry <- fileLbsToEntry lbs fp
    let updatedArchive = Zip.addEntryToArchive entry archive
    pure updatedArchive


replaceMimetype :: Zip.Archive -> IO Zip.Archive
replaceMimetype archive = 
    case Zip.findEntryByPath "mimetype" archive of
        Just mimetype -> pure . Zip.addEntryToArchive mimetype $ archive
        Nothing -> error "No mimetype file present."


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


