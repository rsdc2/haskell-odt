{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module AppendSpec where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Text.XML as X

import Text.ODT

import ConstantsSpec

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

-- Test that appending directly to an ODT yields the same
-- result as appending to a doc
appendToODTEqAppendToDoc :: IO Bool
appendToODTEqAppendToDoc = do
  archive <- loadArchive
  let doc = contentDoc archive
  let odt = getODT doc

  let docodt = getODT $ appendODT [italicPara, boldItalicSpan, italicPara, boldItalicSpan] doc
  let odtodt = odt <> [italicPara, boldItalicSpan, italicPara, boldItalicSpan]

  return $ docodt == odtodt

-- Test that appending directly to an ODT yields the same
-- result as appending to a doc
prependToODTEqAppendToDoc :: IO Bool
prependToODTEqAppendToDoc = do
  archive <- loadArchive
  let doc = contentDoc archive
  let odt = getODT doc

  let docodt = getODT $ prependODT [italicPara, boldItalicSpan, italicPara, boldItalicSpan] doc
  let odtodt = [italicPara, boldItalicSpan, italicPara, boldItalicSpan] <> odt

  return $ docodt == odtodt