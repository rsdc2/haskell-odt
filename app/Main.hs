{-# LANGUAGE OverloadedStrings #-}

import Text.ODT
import Text.ODT.Diagnostics.Utils

import Control.Monad.Writer

minimalExampleWithDiagnostics :: IO () 
minimalExampleWithDiagnostics = do
    let fn = "empty"
    archive <- archiveFromZip "./examples" fn "./.working"
    prettifyODT "./.working" fn

    let contentODT = getContentDocODT archive <> textspan (Just bold) "hello"

    putStrLn . show $ contentODT    
    let archive' = replaceContentDocODT contentODT archive 
    let options = defaultODTFileOptions { workingFolder = Just "./.working", removeWorkingFolder = False, removeWorkingPath = False } 
    updateODTFile archive' "./examples" fn "./examples/output" fn options
    prettifyODT "./.working" fn
    return ()

minimalODTWriter :: Writer ODT ()
minimalODTWriter = do
    textspanM Nothing "hello"
    textspanM Nothing "goodbye"
    return ()

main :: IO ()
main = do
    let fn = "empty"
    archive <- archiveFromZip "./examples" fn "./.working"
    prettifyODT "./.working" fn

    let contentODT = getContentDocODT archive <> (execWriter minimalODTWriter) 

    putStrLn . show $ contentODT    
    let archive' = replaceContentDocODT contentODT archive 
    let options = defaultODTFileOptions { workingFolder = Just "./.working", removeWorkingFolder = False, removeWorkingPath = False } 
    updateODTFile archive' "./examples" fn "./examples/output" fn options
    prettifyODT "./.working" fn
    return ()
 