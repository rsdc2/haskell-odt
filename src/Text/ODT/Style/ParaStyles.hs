{-# LANGUAGE OverloadedStrings #-}

module Text.ODT.Style.ParaStyles (
      normalPara
    , italicPara
    , italicParaM
) 

where

import Control.Monad.Writer
import Text.ODT.Style.Types
import Text.ODT.ODT

normalPara :: ParaStyle
normalPara = newParaStyle

italicPara :: ParaStyle
italicPara = newParaStyle {paraTextProps = newTextProps {fontStyle = Italic}, paraStyleName = Just "italicPara"}

italicParaM :: Writer ODT ()
italicParaM = tell . toODT $ italicPara