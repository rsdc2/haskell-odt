{-# LANGUAGE OverloadedStrings #-}

module AppendSpec where


import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Text.XML as X

import Text.ODT.File
import Text.ODT.XML.Prettify
import qualified Text.ODT.Zip.Zip as Z 
import Text.ODT.Utils.Types (
      IsText(..)
    , Stringable(..))
import Text.ODT.ODT
import Text.ODT.Doc
import Text.ODT.Archive
import qualified Text.ODT.Ops as ODT
import qualified Text.ODT.ODT as ODTType
import Text.ODT.Style

exampleFileName = "example2"


loadArchive :: IO Archive
loadArchive = do
  Z.unzip (path $ exampleFileName <> ".odt") (path $ "/" <> exampleFileName)

  -- Read files
  contentxmldoc <- X.readFile X.def (path $ exampleFileName <> "/content.xml")
  stylesxmldoc <- X.readFile X.def (path $ exampleFileName <> "/styles.xml")

  let contentodtdoc = fromXMLDoc contentxmldoc
  let stylesodtdoc = fromXMLDoc stylesxmldoc

  let archive = Archive {
      contentDoc = contentodtdoc
    , stylesDoc = stylesodtdoc
  } 

  return archive

appendBoldItalicSpecSad :: IO Bool
appendBoldItalicSpecSad = do
  
  archive <- loadArchive
  let boldItalic = newTextStyle {textTextProps = newTextProps {fontStyle = Italic, fontWeight = Bold, fontSize = ""}}
  return $ hasTextStyle boldItalic . contentDoc $ archive


appendBoldItalicSpecHappy :: IO Bool
appendBoldItalicSpecHappy = do
  
  archive <- loadArchive

  let boldItalic = newTextStyle {textTextProps = newTextProps {fontStyle = Italic, fontWeight = Bold, fontSize = ""}}
  let boldItalicODT = ODT.span boldItalic "This is bold and italic text"
  let newcontentdoc = contentDoc . appendODT boldItalicODT $ archive

  return $ hasTextStyle boldItalic newcontentdoc

