{-# LANGUAGE OverloadedStrings #-}

module Examples.SimpleExample (main) where

import Text.ODT
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
    italicwritePara
    return ()

textStyles :: Writer [TextStyle] ()
textStyles = do
    newTextStyleM'
    tell [bold]
    return ()

minimalODT :: Writer ODT ()
minimalODT = do
    writeTextSpan newTextStyle' "Hello"
    writeTextSpan bold " world."
    writePara normalPara ""
    writeTextSpan underline "This text is underlined."
    writePara italicPara "This text is italic because it is in an italic paragraph."
    writePara normalPara "This text is normal."

main :: IO ()
main = do
    writeNewODTWithStyles "./examples/output" "StylesExample.odt" paraStyles textStyles minimalODT
