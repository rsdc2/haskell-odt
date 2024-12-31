module Text.ODT.Compress (saveToArchive) where

import qualified Text.ODT.Zip.Zip as Zip 
import qualified Text.XML as XML

import Text.ODT.Archive ( Archive(..) )
import Text.ODT.Doc ( IsXMLDoc(toXMLDoc) )

type SrcFolderPath = String
type DstFolderPath = String
type Filename = String


saveToArchive :: Archive -> SrcFolderPath -> Filename -> DstFolderPath ->  IO ()
saveToArchive archive orig fn dst = do
    let dstpath = dst <> "/" <> fn
    let srcpath = orig <> "/" <> fn

    XML.writeFile XML.def (dstpath <> "/content.xml") (toXMLDoc . contentDoc $ archive)
    XML.writeFile XML.def (dstpath <> "/styles.xml") (toXMLDoc . stylesDoc $ archive) 

    Zip.zipODT (srcpath <> ".odt") [dstpath <> "/content.xml", dstpath <> "/styles.xml"] (dst <>  "/modified.odt")
