module Text.ODT.Zip.Utils
    ( unzipFiles ) where

import qualified Text.ODT.Zip.Zip as Zip
import Text.ODT.XML.Prettify ( prettifyFile )

type DestFolderpath = String
type ODTFolderpath = String
type ODTFilename = String

-- Unzip odt file and write prettified xml files to dstpath
-- param odtpath: folderpath containing .odt file
-- param filename: filename of .odt file without .odt extension
-- param dstpath: destination folder of folder containing archive to be unzipped
unzipFiles :: ODTFolderpath -> ODTFilename -> DestFolderpath -> IO ()
unzipFiles odtpath filename dstpath = do
    Zip.unzip (odtpath <> "/" <> filename <> ".odt") (dstpath <> "/" <> filename)
    prettifyFile (dstpath <> "/" <> filename <> "/styles.xml") (dstpath <> "/prettified/styles.xml")
    prettifyFile (dstpath <> "/" <> filename <> "/content.xml") (dstpath <> "/prettified/content.xml")
