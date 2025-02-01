{-# LANGUAGE OverloadedStrings #-}

import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as ByteS 
import Text.ODT.Zip.Zip
import Text.ODT.Doc

import Text.ODT
import Text.ODT.Diagnostics

import Control.Monad.Writer

-- printFiles :: IO ()
-- printFiles = do
--     file <- ByteS.readFile "./examples/example2.odt"
--     let archive = toArchive file
--     let files = filesInArchive archive
--     putStrLn . show $ files


-- main :: IO ()
-- main = do
--     file <- ByteS.readFile "./examples/example2.odt"
--     let archive = toArchive file
--     let entry = findEntryByPath "content.xml" archive
--     let decompressed = fromEntry <$> entry
--     putStrLn . show $ decompressed



-- main :: IO ()
-- main = do
--     lbs <- fileLBSFromZip "./examples/example2.odt" "content.xml"
--     let odt = odtFromXmlLBS lbs
--     putStrLn . show $ odt
         

newTextStyle' :: TextStyle
newTextStyle' = newTextStyle {textTextProps = newTextProps {fontStyle = Italic}, textStyleName = Just "newstyle"}

newTextStyleM' :: Writer [TextStyle] ()
newTextStyleM' = tell [newTextStyle']

setupStyles :: Writer ODT ()
setupStyles = do
    italicParaODTM

-- setupStyles' :: (IsStyle a, IsODT a) => Writer [a] ()
paraStyles :: Writer [ParaStyle] ()
paraStyles = do 
    italicParaM
    return ()

textStyles :: Writer [TextStyle] ()
textStyles = do
    newTextStyleM'
    return ()

minimalODT :: Writer ODT ()
minimalODT = do
    textspanM newTextStyle' "Hello"
    textspanM bold " world."
    paraM normalPara ""
    textspanM underline "This text is underlined."
    paraM italicPara "This text is italic because it is in an italic paragraph."

main :: IO ()
main = do
    saveNewODT "./examples/output" "newodt_mod.odt" minimalODT
