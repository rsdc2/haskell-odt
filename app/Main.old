{-# LANGUAGE OverloadedStrings #-}

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

-- setupStyles' :: (IsStyle a, IsODT a) => Writer [a] ()
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

main :: IO ()
main = do
    saveNewODTWithStylesDiag' "./examples/output" "newodt" minimalODT paraStyles textStyles
