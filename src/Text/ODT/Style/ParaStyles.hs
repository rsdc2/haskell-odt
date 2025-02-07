{-# LANGUAGE OverloadedStrings #-}

module Text.ODT.Style.ParaStyles (
      normalPara
    , italicPara
    , italicwritePara
    , italicParaODTM
) 

where

import Control.Monad.Writer
import Text.ODT.Style.Types
import Text.ODT.ODT

normalPara :: ParaStyle
normalPara = newParaStyle

italicPara :: ParaStyle
italicPara = newParaStyle {paraTextProps = newTextProps {fontStyle = Italic}, paraStyleName = Just "italicPara"}

italicParaODTM :: Writer ODT ()
italicParaODTM = tell . toODT $ italicPara

italicwritePara :: Writer [ParaStyle] ()
italicwritePara = tell [italicPara]