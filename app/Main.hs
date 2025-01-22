{-# LANGUAGE OverloadedStrings #-}

import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as ByteS 
import Text.ODT.Zip.Zip
import Text.ODT.Doc

printFiles :: IO ()
printFiles = do
    file <- ByteS.readFile "./examples/example2.odt"
    let archive = toArchive file
    let files = filesInArchive archive
    putStrLn . show $ files


-- main :: IO ()
-- main = do
--     file <- ByteS.readFile "./examples/example2.odt"
--     let archive = toArchive file
--     let entry = findEntryByPath "content.xml" archive
--     let decompressed = fromEntry <$> entry
--     putStrLn . show $ decompressed



main :: IO ()
main = do
    lbs <- fileLBSFromZip "./examples/example2.odt" "content.xml"
    let odt = odtFromXmlLBS lbs
    putStrLn . show $ odt
         
