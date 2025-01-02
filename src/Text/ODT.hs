module Text.ODT (
      Archive(..)
    , archiveFromZip
    , defaultODTFileOptions
    , Doc(..)
    , getFirstODT
    , getFirstPara
    , getLastPara
    , getParas
    , getParaStyleNamesFromParaNodes
    , getParaStylesWithName
    , getSpans
    , getText
    , getTextStyleNamesFromParaNodes
    , HasContentODT(..)
    , HasODT(..)
    , HasParaStyles(..)
    , HasTextStyles(..)
    , IsODT(..)
    , ODT(..)
    , ODTFileOptions(..)
    , paraCount
    , ParaStyle(..)
    , replaceContentDocODT
    , spanCount
    , updateODTFile
    , bold
    , italic
    , textspan
    , underline
    ) where

import Text.ODT.Extract
import Text.ODT.Compress
import Text.ODT.File
import Text.ODT.XML.Prettify
import Text.ODT.Utils.Types (
      IsText(..)
    , Stringable(..))
import Text.ODT.ODT
import Text.ODT.Doc
import Text.ODT.Archive
import Text.ODT.Query
import Text.ODT.TextUnits
import qualified Text.ODT.ODT as ODTType
import Text.ODT.Style
import Text.ODT.Style.TextStyles



