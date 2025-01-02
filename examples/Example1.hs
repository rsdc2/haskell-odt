{-# LANGUAGE OverloadedStrings #-}

import Text.ODT

main :: IO ()
main = do
    archive <- archiveFromZip "./examples" "empty" "./.working"
    let contentODT = getContentDocODT archive <> textspan underline " Some underlined text." 
    let archive' = replaceContentDocODT contentODT archive
    let options = defaultODTFileOptions { workingFolder = Just "./.working", removeWorkingFolder = False, removeWorkingPath = False } 
    updateODTFile archive' "./examples" "empty" "./examples/output" "modified" options
    return ()
