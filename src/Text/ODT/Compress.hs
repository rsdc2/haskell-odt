module Text.ODT.Compress (saveToArchive) where

import qualified Text.ODT.Zip.Zip as Zip 
import qualified Text.XML as XML

import Text.ODT.Archive ( Archive(..) )
import Text.ODT.Doc ( IsXMLDoc(toXMLDoc) )

type SrcFolderPath = String
type DstFolderPath = String
type Filename = String


saveToArchive :: Archive -> SrcFolderPath -> DstFolderPath -> Filename -> IO ()
saveToArchive archive orig dst fn = do
    let rootpath = dst <> "/" <> fn

    XML.writeFile XML.def (rootpath <> "/content.xml") (toXMLDoc . contentDoc $ archive)
    XML.writeFile XML.def (rootpath <> "/styles.xml") (toXMLDoc . stylesDoc $ archive) 

    Zip.zipODT (rootpath <> ".odt") [rootpath <> "/content.xml", rootpath <> "/styles.xml"] (dst <>  "/modified.odt")
