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
    paraM normalPara "This text is normal."

main :: IO ()
main = do
    saveNewODT "./examples/output" "newodt_mod.odt" minimalODT
