module Text.ODT.Diagnostics.Utils
    ( prettifyODT ) where

import System.Directory (createDirectoryIfMissing)
import Text.ODT.XML.Prettify ( prettifyFile )

type SrcFolderPath = String
type DstFolderPath = String
type Filename = String

-- 
prettifyODT :: SrcFolderPath -> Filename -> IO ()
prettifyODT folderpath filename = 
    prettifyFile (folderpath <> "/" <> filename <> "/styles.xml") (folderpath <> "/prettified/styles.xml") >>
    prettifyFile (folderpath <> "/" <> filename <> "/content.xml") (folderpath <> "/prettified/content.xml")


    
