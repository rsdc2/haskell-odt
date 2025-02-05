{-# LANGUAGE OverloadedStrings #-}

module Examples.SimpleExample (main) where

import Text.ODT
import Control.Monad.Writer
         

newTextStyle' :: TextStyle
newTextStyle' = newTextStyle {textTextProps = newTextProps {fontStyle = Italic}, textStyleName = Just "newstyle"}

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
    writeNewODT "./examples/output" "SimpleExample.odt" minimalODT
