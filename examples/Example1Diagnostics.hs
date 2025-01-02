{-# LANGUAGE OverloadedStrings #-}

import Text.ODT
import Text.ODT.Diagnostics.Utils

main :: IO ()
main = do
    let fn = "empty"
    archive <- archiveFromZip "./examples" fn "./.working"
    prettifyODT "./.working" fn

    let contentODT = getContentDocODT archive <> para Nothing "" <> textspan Nothing "hello" <> para Nothing "" <> textspan Nothing "goodbye"
    let archive' = replaceContentDocODT contentODT archive 
    let options = defaultODTFileOptions { workingFolder = Just "./.working", removeWorkingFolder = False, removeWorkingPath = False } 
    updateODTFile archive' "./examples" fn "./examples/output" fn options
    prettifyODT "./.working" fn
    return ()
 