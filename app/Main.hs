{-# LANGUAGE OverloadedStrings #-}

import Text.ODT
import Text.ODT.Diagnostics

import Control.Monad.Writer

setupStyles :: Writer ODT ()
setupStyles = do
    italicParaM

minimalODT :: Writer ODT ()
minimalODT = do
    textspanM italic "Hello"
    textspanM bold " world."
    paraM normalPara ""
    textspanM underline "This text is underlined."
    paraM italicPara "This text is italic because it is in an italic paragraph."

main :: IO ()
main = do
    saveNewODTWithStylesDiag "./examples/output" "newodt" minimalODT setupStyles
