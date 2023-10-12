{-# LANGUAGE OverloadedStrings #-}

module Text.ODT.XML.Attrs (
      noAttrs
    , xmlSpaceName
    , xmlSpacePreserve
)

where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe (Maybe(..))
import Text.XML (Name(..))

import Text.ODT.ODTXML.Name (toName)

noAttrs = Map.empty

xmlSpaceName :: Name
xmlSpaceName = Name "xml:space" Nothing (Just "xml")

xmlSpacePreserve :: Map.Map Name T.Text
xmlSpacePreserve = Map.fromList [(xmlSpaceName, "preserve")]