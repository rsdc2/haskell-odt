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
  let newcontentdoc = contentDoc . appendODT boldItalicSpan $ archive
  return $ hasTextStyle boldItalicStyle newcontentdoc

prependBoldItalicTextStyleWithSpanHappy :: IO Bool
prependBoldItalicTextStyleWithSpanHappy = do
  archive <- loadArchive
  let newcontentdoc = contentDoc . prependODT boldItalicSpan $ archive
  return $ hasTextStyle boldItalicStyle newcontentdoc

appendItalicParaStyleWithParaSad :: IO Bool
appendItalicParaStyleWithParaSad =  hasParaStyle italicParaStyle . contentDoc <$> loadArchive

appendItalicParaStyleWithParaHappy :: IO Bool
appendItalicParaStyleWithParaHappy = do
  archive <- loadArchive
  let newcontentdoc = contentDoc . appendODT italicPara $ archive

  let origParaCount = length . getParas . getODT . contentDoc $ archive
  let newParaCount = length . getParas . getODT $ newcontentdoc

  return $ hasParaStyle italicParaStyle newcontentdoc && newParaCount == origParaCount + 1

prependItalicParaStyleWithParaHappy :: IO Bool
prependItalicParaStyleWithParaHappy = do
  archive <- loadArchive
  let newcontentdoc = contentDoc . prependODT italicPara $ archive

  let origParaCount = length . getParas . getODT . contentDoc $ archive
  let newParaCount = length . getParas . getODT $ newcontentdoc

  return $ hasParaStyle italicParaStyle newcontentdoc && newParaCount == origParaCount + 1
