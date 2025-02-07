{-# LANGUAGE OverloadedStrings #-}

module Examples.SimpleExample (main) where

import Text.ODT
import Control.Monad.Writer
         

newTextStyle' :: TextStyle
newTextStyle' = newTextStyle {textTextProps = newTextProps {fontStyle = Italic}, textStyleName = Just "newstyle"}

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
    writeNewODT "./examples/output" "SimpleExample.odt" minimalODT
