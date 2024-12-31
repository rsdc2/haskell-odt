module Text.ODT.Compress (updateArchive) where

import System.Directory ( removeDirectoryRecursive )
import qualified Text.ODT.Zip.Zip as Zip 
import qualified Text.XML as XML

import Text.ODT.Archive ( Archive(..) )
import Text.ODT.Doc ( IsXMLDoc(toXMLDoc) )

type SrcFolderPath = String
type DstFolderPath = String
type Filename = String


updateArchive :: Archive -> SrcFolderPath -> Filename -> DstFolderPath -> Filename -> IO ()
updateArchive archive origFolder origFn dstFolder dstFn = do
    let dstpath = dstFolder <> "/" <> dstFn
    let origpath = origFolder <> "/" <> origFn

    XML.writeFile XML.def (origpath <> "/content.xml") (toXMLDoc . contentDoc $ archive)
    XML.writeFile XML.def (origpath <> "/styles.xml") (toXMLDoc . stylesDoc $ archive) 
    Zip.zipODT (origpath <> ".odt") [origpath <> "/content.xml", origpath <> "/styles.xml"] (dstFolder <>  "/" <> dstFn <> ".odt")
    removeDirectoryRecursive origpath
