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

boldItalicStyle :: TextStyle
boldItalicStyle = newTextStyle {textTextProps = newTextProps {fontStyle = Italic, fontWeight = Bold, fontSize = ""}}

boldItalicSpan :: ODT
boldItalicSpan = ODT.span boldItalicStyle "This is bold and italic text"

italicParaStyle :: ParaStyle
italicParaStyle = newParaStyle {paraTextProps = newTextProps {fontStyle = Italic}, paraStyleName = Just "italicPara"}

italicPara :: ODT
italicPara = ODT.p italicParaStyle "Italic para style"

appendBoldItalicTextStyleWithSpanSad :: IO Bool
appendBoldItalicTextStyleWithSpanSad =  hasTextStyle boldItalicStyle . contentDoc <$> loadArchive

appendBoldItalicTextStyleWithSpanHappy :: IO Bool
appendBoldItalicTextStyleWithSpanHappy = do
  archive <- loadArchive
  let orig = contentDoc archive
  let new = appendODT boldItalicSpan orig
  return $ not (hasTextStyle boldItalicStyle orig)
            && getText orig == "Hello worldThis is underlined.This is italic"
            && hasTextStyle boldItalicStyle new
            && spanCount new == spanCount orig + 1
            && getText new == "Hello worldThis is underlined.This is italicThis is bold and italic text"

prependBoldItalicTextStyleWithSpanHappy :: IO Bool
prependBoldItalicTextStyleWithSpanHappy = do
  archive <- loadArchive
  let orig = contentDoc archive
  let new = prependODT boldItalicSpan orig
  return $ not (hasTextStyle boldItalicStyle orig)
            && getText orig == "Hello worldThis is underlined.This is italic"
            && hasTextStyle boldItalicStyle new
            && spanCount new == spanCount orig + 1
            && getText new == "This is bold and italic textHello worldThis is underlined.This is italic"

appendItalicParaStyleWithParaSad :: IO Bool
appendItalicParaStyleWithParaSad =  hasParaStyle italicParaStyle . contentDoc <$> loadArchive

appendItalicParaStyleWithParaHappy :: IO Bool
appendItalicParaStyleWithParaHappy = do
  archive <- loadArchive
  let orig = contentDoc archive
  let new = appendODT italicPara orig

  return $ not (hasParaStyle italicParaStyle orig)
            && getText orig == "Hello worldThis is underlined.This is italic"
            && hasParaStyle italicParaStyle new 
            && paraCount new == paraCount orig + 1
            && getText new == "Hello worldThis is underlined.This is italicItalic para style"

prependItalicParaStyleWithParaHappy :: IO Bool
prependItalicParaStyleWithParaHappy = do
  archive <- loadArchive
  let orig = contentDoc archive
  let new = prependODT italicPara orig

  return $ not (hasParaStyle italicParaStyle orig)
            && getText orig == "Hello worldThis is underlined.This is italic"
            && hasParaStyle italicParaStyle new 
            && paraCount new == paraCount orig + 1
            && getText new == "Italic para styleHello worldThis is underlined.This is italic"
