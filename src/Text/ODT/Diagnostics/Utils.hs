module Text.ODT.Diagnostics.Utils
    ( prettifyODT ) where

import System.Directory (createDirectoryIfMissing)
import Text.ODT.XML.Prettify ( prettifyFile )

type SrcFolderPath = String
type DstFolderPath = String
type Filename = String

-- 
prettifyODT :: SrcFolderPath -> Filename -> IO ()
prettifyODT folderpath filename = do
    createDirectoryIfMissing True (folderpath <> "/" <> filename <> "_prettified")
    prettifyFile (folderpath <> "/" <> filename <> "/styles.xml") (folderpath <> "/" <> filename <> "_prettified/styles.xml")
    prettifyFile (folderpath <> "/" <> filename <> "/content.xml") (folderpath <> "/" <> filename <> "_prettified/content.xml")


    
