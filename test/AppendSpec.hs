{-# LANGUAGE OverloadedStrings #-}

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

