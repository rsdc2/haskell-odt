module Text.ODT.Diagnostics.Utils
    ( unzipOdt, prettifyOdt ) where

import System.Directory (createDirectoryIfMissing)
import qualified Text.ODT.Zip.Zip as Zip
import Text.ODT.XML.Prettify ( prettifyFile )

type SrcFolderPath = String
type DstFolderPath = String
type Filename = String

-- Unzip odt file and write prettified xml files to dstpath
-- param odtpath: folderpath containing .odt file
-- param filename: filename of .odt file without .odt extension
-- param dstpath: destination folder of folder containing archive to be unzipped
unzipOdt :: SrcFolderPath -> Filename -> DstFolderPath -> IO ()
unzipOdt srcpath filename dstpath = 
    Zip.unzip (srcpath <> "/" <> filename <> ".odt") (dstpath <> "/" <> filename)

-- 
prettifyOdt :: SrcFolderPath -> Filename -> IO ()
prettifyOdt folderpath filename = 
    prettifyFile (folderpath <> "/" <> filename <> "/styles.xml") (folderpath <> "/prettified/styles.xml") >>
    prettifyFile (folderpath <> "/" <> filename <> "/content.xml") (folderpath <> "/prettified/content.xml")


    
