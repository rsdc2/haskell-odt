module Text.ODT.Extract (archiveFromZip) where


import qualified Text.ODT.Zip.Zip as Zip 
import qualified Text.XML as Xml

import Text.ODT.Archive ( Archive(..) )
import Text.ODT.Doc ( IsXMLDoc(fromXMLDoc) )

type SrcFolderPath = String
type DstFolderPath = String
type Filename = String

-- Helper functions for extracting ODT files

loadArchive :: SrcFolderPath -> Filename -> IO Archive
loadArchive src fn = do
  Zip.unzip (src <> "/" <> fn <> ".odt") (src <> "/" <> fn)

  -- Read files
  contentxmldoc <- Xml.readFile Xml.def (src <> "/" <> fn <> "/content.xml")
  stylesxmldoc <- Xml.readFile Xml.def (src <> "/" <> fn <> "/styles.xml")

  let contentodtdoc = fromXMLDoc contentxmldoc
  let stylesodtdoc = fromXMLDoc stylesxmldoc

  return Archive {
      contentDoc = contentodtdoc
    , stylesDoc = stylesodtdoc
  } 

archiveFromZip :: SrcFolderPath -> Filename -> DstFolderPath -> IO Archive
archiveFromZip srcpath filename dstpath = do
    Zip.unzipOdt srcpath filename dstpath
    archive <- loadArchive srcpath filename
    return archive