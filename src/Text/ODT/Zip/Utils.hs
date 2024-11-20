module Text.ODT.Zip.Utils
    ( unzipFiles ) where

import qualified Text.ODT.Zip.Zip as Zip
import Text.ODT.XML.Prettify ( prettifyFile )

type DestFolderpath = String
type ODTFilepath = String
type Filename = String

-- Unzip odt file and output prettified xml files to dstpath
unzipFiles :: ODTFilepath -> Filename -> DestFolderpath -> IO ()
unzipFiles odtpath filename dstpath = do
    Zip.unzip (odtpath <> filename <> ".odt") (dstpath <> "/" <> filename)
    prettifyFile (dstpath <> "/" <> filename <> "/styles.xml") (dstpath <> "styles_pretty.xml")
    prettifyFile (dstpath <> "/" <> "content.xml") (dstpath <> "/" <> "content_pretty.xml")
