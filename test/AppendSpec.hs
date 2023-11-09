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
import Text.ODT.Query
import qualified Text.ODT.Ops as ODT
import qualified Text.ODT.ODT as ODTType
import Text.ODT.Style

exampleFileName = "example"
inputPath = "test/files/input/"

loadArchive :: IO Archive
loadArchive = do
  Z.unzip (inputPath <> exampleFileName <> ".odt") (inputPath <> exampleFileName)

  -- Read files
  contentxmldoc <- X.readFile X.def (inputPath <> exampleFileName <> "/content.xml")
  stylesxmldoc <- X.readFile X.def (inputPath <> exampleFileName <> "/styles.xml")

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
boldItalicSpan = ODT.span boldItalicStyle testText

italicParaStyle :: ParaStyle
italicParaStyle = newParaStyle 
  { paraTextProps = newTextProps {fontStyle = Italic}
  , paraStyleName = Just "italicPara" }

italicPara :: ODT
italicPara = ODT.p italicParaStyle testText

appendBoldItalicTextStyleWithSpanSad :: IO Bool
appendBoldItalicTextStyleWithSpanSad = 
  hasTextStyle boldItalicStyle . contentDoc <$> loadArchive

appendBoldItalicTextStyleWithSpanHappy :: IO Bool
appendBoldItalicTextStyleWithSpanHappy = do
  archive <- loadArchive
  let orig = contentDoc archive
  let new = appendODT boldItalicSpan orig
  return $ not (hasTextStyle boldItalicStyle orig)
            && hasTextStyle boldItalicStyle new
            && spanCount new == spanCount orig + 1
            && getText orig /= ""
            && getText new == getText orig <> testText

prependBoldItalicTextStyleWithSpanHappy :: IO Bool
prependBoldItalicTextStyleWithSpanHappy = do
  archive <- loadArchive
  let orig = contentDoc archive
  let new = prependODT boldItalicSpan orig
  return $ not (hasTextStyle boldItalicStyle orig)
            && hasTextStyle boldItalicStyle new
            && spanCount new == spanCount orig + 1
            && getText orig /= ""
            && getText new == testText <> getText orig

appendItalicParaStyleWithParaSad :: IO Bool
appendItalicParaStyleWithParaSad = 
  hasParaStyle italicParaStyle . contentDoc <$> loadArchive

appendItalicParaStyleWithParaHappy :: IO Bool
appendItalicParaStyleWithParaHappy = do
  archive <- loadArchive
  let orig = contentDoc archive
  let new = appendODT italicPara orig

  return $ not (hasParaStyle italicParaStyle orig)
            && hasParaStyle italicParaStyle new 
            && paraCount new == paraCount orig + 1
            && getText orig /= ""
            && getText new == getText orig <> testText

prependItalicParaStyleWithParaHappy :: IO Bool
prependItalicParaStyleWithParaHappy = do
  archive <- loadArchive
  let orig = contentDoc archive
  let new = prependODT italicPara orig

  return $ not (hasParaStyle italicParaStyle orig)
            && hasParaStyle italicParaStyle new 
            && paraCount new == paraCount orig + 1
            && getText orig /= ""
            && getText new == testText <> getText orig
