{-# LANGUAGE OverloadedStrings #-}

import Text.ODT
import Text.ODT.Style.Types

import Control.Monad.Writer

italicTextStyle :: TextStyle
italicTextStyle = newTextStyle {textTextProps = newTextProps {fontStyle = Italic}, textStyleName = Just "newstyle"}

boldTextStyle :: TextStyle
boldTextStyle = newTextStyle {textTextProps = newTextProps {fontWeight = Bold}, textStyleName = Just "newBold"}

paraStyles :: Writer [ParaStyle] ()
paraStyles = tell [italicPara]

textStyles :: Writer [TextStyle] ()
textStyles = tell [italicTextStyle, boldTextStyle]

content :: Writer ODT ()
content = do
    writeTextSpan italicTextStyle "Hello"
    writeTextSpan boldTextStyle " world."
    writeNewPara
    writeTextSpan underline "This text is underlined."
    writePara italicPara "This text is italic because it is in an italic paragraph."
    writePara normalPara "This text is normal."

simpleExample :: IO ()
simpleExample = do
    writeNewODT "./examples/output" "SimpleExample.odt" content

stylesExample :: IO ()
stylesExample = do
    writeNewODTWithStyles "./examples/output" "StylesExample.odt" paraStyles textStyles content 

main :: IO ()
main = do
    stylesExample
