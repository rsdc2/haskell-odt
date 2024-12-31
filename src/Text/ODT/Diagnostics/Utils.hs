module Text.ODT.Diagnostics.Utils
    ( prettifyOdt ) where

import System.Directory (createDirectoryIfMissing)
import Text.ODT.XML.Prettify ( prettifyFile )

type SrcFolderPath = String
type DstFolderPath = String
type Filename = String

-- 
prettifyOdt :: SrcFolderPath -> Filename -> IO ()
prettifyOdt folderpath filename = 
    prettifyFile (folderpath <> "/" <> filename <> "/styles.xml") (folderpath <> "/prettified/styles.xml") >>
    prettifyFile (folderpath <> "/" <> filename <> "/content.xml") (folderpath <> "/prettified/content.xml")


    
