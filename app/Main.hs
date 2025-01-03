{-# LANGUAGE OverloadedStrings #-}

import Text.ODT
import Text.ODT.Diagnostics

import Control.Monad.Writer

minimalODT :: Writer ODT ()
minimalODT = do
    textspanM (Just italic) "Hello"
    textspanM (Just bold) " world."

main :: IO ()
main = saveNewODTDiag "./examples/output" "newodt" minimalODT
