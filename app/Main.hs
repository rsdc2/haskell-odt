{-# LANGUAGE OverloadedStrings #-}

import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as ByteS 
import Text.ODT.Zip.Zip
import Text.ODT.Doc

import Text.ODT
import Text.ODT.Diagnostics

import Control.Monad.Writer

newTextStyle' :: TextStyle
newTextStyle' = newTextStyle {textTextProps = newTextProps {fontStyle = Italic}, textStyleName = Just "newstyle"}

newTextStyleM' :: Writer [TextStyle] ()
newTextStyleM' = tell [newTextStyle']

setupStyles :: Writer ODT ()
setupStyles = do
    italicParaODTM

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
    paraM normalPara "This text is normal."

main :: IO ()
main = do
    saveNewODT "./examples/output" "newodt_mod.odt" minimalODT
