module Text.ODT (
      Archive(..)
    , Doc(..)
    , getFirstPara
    , getLastPara
    , getParas
    , getParaStyleNamesFromParaNodes
    , getSpans
    , getText
    , getTextStyleNamesFromParaNodes
    , HasODT(..)
    , HasParaStyles(..)
    , HasTextStyles(..)
    , IsODT(..)
    , ODT(..)
    , paraCount
    , ParaStyle(..)
    , spanCount
    ) where

import Text.ODT.File
import Text.ODT.XML.Prettify
import qualified Text.ODT.Zip.Zip as Z 
import Text.ODT.Utils.Types (
      IsText(..)
    , Stringable(..))
import Text.ODT.ODT
import Text.ODT.Doc
import Text.ODT.Archive
import Text.ODT.Query
import qualified Text.ODT.Ops as ODT
import qualified Text.ODT.ODT as ODTType
import Text.ODT.Style