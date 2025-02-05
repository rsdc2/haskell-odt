{-# LANGUAGE OverloadedStrings #-}

module ConstantsSpec where

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
import Text.ODT.Doc ( IsXMLDoc(fromXMLDoc) )
import Text.ODT.Archive
import Text.ODT.Query
import Text.ODT.TextUnits
import qualified Text.ODT.ODT as ODTType
import Text.ODT.Style

inputPath = "test/files/input/"

loadArchive :: String -> IO Archive
loadArchive filename = do
  Z.unzip (inputPath <> filename <> ".odt") (inputPath <> filename)

  -- Read files
  contentxmldoc <- X.readFile X.def (inputPath <> filename <> "/content.xml")
  stylesxmldoc <- X.readFile X.def (inputPath <> filename <> "/styles.xml")

  let contentodtdoc = fromXMLDoc contentxmldoc
  let stylesodtdoc = fromXMLDoc stylesxmldoc

  let archive = Archive {
      contentDoc = contentodtdoc
    , stylesDoc = stylesodtdoc
  } 

  return archive

testText :: T.Text
testText = "test text"

boldItalicStyle :: TextStyle
boldItalicStyle = newTextStyle 
  { textTextProps = newTextProps {fontStyle = Italic
  , fontWeight = Bold, fontSize = ""} }

boldItalicSpan :: ODT
boldItalicSpan = textspan (boldItalicStyle) testText

italicParaStyle :: ParaStyle
italicParaStyle = newParaStyle 
  { paraTextProps = newTextProps {fontStyle = Italic}
  , paraStyleName = Just "italicPara" }

italicPara :: ODT
italicPara = para (italicParaStyle) testText

odtList :: [ODT]
odtList = [italicPara, boldItalicSpan]
