{-# LANGUAGE OverloadedStrings #-}

import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as ByteS 
import Text.ODT.Zip.Zip
import Text.ODT.Doc

import Text.ODT
import Text.ODT.Diagnostics
import Text.ODT.Style.Types

import Control.Monad.Writer

newTextStyle' :: TextStyle
newTextStyle' = newTextStyle {textTextProps = newTextProps {fontStyle = Italic}, textStyleName = Just "newstyle"}

newBold :: TextStyle
newBold = newTextStyle {textTextProps = newTextProps {fontWeight = Bold}, textStyleName = Just "newBold"}


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
    tell [newBold]
    return ()

minimalODT :: Writer ODT ()
minimalODT = do
    textspanM newTextStyle' "Hello"
    textspanM newBold " world."
    paraM normalPara ""
    textspanM underline "This text is underlined."
    paraM italicPara "This text is italic because it is in an italic paragraph."
    paraM normalPara "This text is normal."

simpleExample :: IO ()
simpleExample = do
    writeNewODT "./examples/output" "SimpleExample.odt" minimalODT

stylesExample :: IO ()
stylesExmaple = do
    writeNewODTWithStyles "./examples/output" "StylesExample.odt" paraStyles textStyles minimalODT 

main :: IO ()
main = do
    stylesExample
