module Text.ODT.Extract (loadArchiveFromZip, extractAndLoadArchiveFromZip) where


import qualified Text.ODT.Zip.Zip as Zip 
import qualified Text.XML as Xml

import Text.ODT.Archive ( Archive(..) )
import Text.ODT.Doc ( IsXMLDoc(fromXMLDoc), docFromXmlLBS)
import qualified Data.ByteString.Lazy as LBS
import Control.Exception.Base

type SrcFolderPath = String
type DstFolderPath = String
type Filename = String

-- Helper functions for extracting ODT files

-- Load ODT file from already decompressed XML archive
loadArchive :: SrcFolderPath -> Filename -> IO Archive
loadArchive src fn = do

  -- Read files
  contentxmldoc <- Xml.readFile Xml.def (src <> "/" <> fn <> "/content.xml")
  stylesxmldoc <- Xml.readFile Xml.def (src <> "/" <> fn <> "/styles.xml")

  let contentodtdoc = fromXMLDoc contentxmldoc
  let stylesodtdoc = fromXMLDoc stylesxmldoc

  return Archive {
      contentDoc = contentodtdoc
    , stylesDoc = stylesodtdoc
  } 

-- Load ODT archive directly from zip file, without decompressing first
loadArchiveFromZip :: SrcFolderPath -> Filename -> IO Archive
loadArchiveFromZip src fn = do 
  contentXmlLbs <- Zip.fileLBSFromZip (src <> "/" <> fn) "content.xml"
  stylesXmlLbs <- Zip.fileLBSFromZip (src <> "/" <> fn) "styles.xml"

  return Archive {
      contentDoc = docFromXmlLBS contentXmlLbs
    , stylesDoc = docFromXmlLBS stylesXmlLbs
  } 


-- Extract ODT file into folder and then load the ODT archive
extractAndLoadArchiveFromZip :: SrcFolderPath -> Filename -> DstFolderPath -> IO Archive
extractAndLoadArchiveFromZip srcpath filename dstpath = do
    Zip.unzipOdt srcpath filename dstpath
    archive <- loadArchive dstpath filename
    return archive