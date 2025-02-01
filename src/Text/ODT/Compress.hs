module Text.ODT.Compress (
    updateODTFile
  , ODTFileOptions(..)
  , defaultODTFileOptions
  , updateODTFileWithExtraction) where

import System.Directory ( removeDirectoryRecursive )
import qualified Text.ODT.Zip.Zip as Zip 
import qualified Text.XML as XML

import Text.ODT.Archive ( Archive(..) )
import Text.ODT.Doc
import qualified Data.ByteString.Lazy as LBS

type Folderpath = String
type Filename = String


data ODTFileOptions = ODTFileOptions { 
      workingFolder :: Maybe Folderpath
    , removeWorkingFolder :: Bool
    , removeWorkingPath :: Bool
}

defaultODTFileOptions = ODTFileOptions {
      workingFolder = Nothing
    , removeWorkingFolder = False
    , removeWorkingPath = True }

cleanupFolders :: ODTFileOptions -> Filename -> IO () 
cleanupFolders ODTFileOptions { workingFolder = Just path, removeWorkingFolder = True } _ = removeDirectoryRecursive path
cleanupFolders ODTFileOptions { workingFolder = Just path, removeWorkingPath = True } fn = do 
  removeDirectoryRecursive (path <> "/" <> fn)
cleanupFolders _ _ = return ()

getWorkingPath :: ODTFileOptions -> Folderpath -> Filename -> Folderpath
getWorkingPath ODTFileOptions { workingFolder = Just suppliedPath } _ fn = suppliedPath <> "/" <> fn

-- Updates an ODT zip file by first writing out the file and then zipping it up
updateODTFileWithExtraction :: Archive -> Folderpath -> Filename -> Folderpath -> Filename -> ODTFileOptions -> IO ()
updateODTFileWithExtraction archive odtFolder odtFn dstFolder dstFn options = do

  let dstPath = dstFolder <> "/" <> dstFn
  let odtPath = odtFolder <> "/" <> odtFn
  let workingPath = getWorkingPath options odtFolder odtFn

  XML.writeFile XML.def (workingPath <> "/content.xml") (toXMLDoc . contentDoc $ archive)
  XML.writeFile XML.def (workingPath <> "/styles.xml") (toXMLDoc . stylesDoc $ archive) 
  Zip.zipODT (odtPath <> ".odt") [workingPath <> "/content.xml", workingPath <> "/styles.xml"] (dstFolder <>  "/" <> dstFn <> ".odt")

  cleanupFolders options odtFn

-- Updates an existing ODT Zip archive with the content and styles from an ODT Archive
updateODTFile :: Archive -> FilePath -> FilePath -> IO ()
updateODTFile odtArchive src dst = do
  let contentLBS = docToXmlLBS . contentDoc $ odtArchive
  let stylesLBS = docToXmlLBS . stylesDoc $ odtArchive

  zipArchive <- Zip.archiveFromFile src
  zipArchiveWithContent <- Zip.saveFileLbsToZipArchive contentLBS "content.xml" zipArchive
  zipArchiveWithStyles <- Zip.saveFileLbsToZipArchive stylesLBS "styles.xml" zipArchiveWithContent

  let zipLBS = Zip.archiveToZipLBS zipArchiveWithStyles
  LBS.writeFile dst zipLBS 

