{-# LANGUAGE OverloadedStrings #-}

module Text.ODT.ODTXML.Attr (
      AttrVal
    , AttrKey
    , IsTextPropAttrMap(..)
    , IsNodeStyleAttrMap(..)
)

where

import Text.XML ( Element(..), Name )
import Data.Text as T
import qualified Data.Map as Map

import Text.ODT.ODTXML.Namespace 
import Text.ODT.ODTXML.Name 

type AttrKey = T.Text
type AttrVal = T.Text


-- Used for defining new styles
class IsTextPropAttrMap a where
    toTextPropAttrMap :: a -> Map.Map Name T.Text

-- Used in assigning text to a style
class IsNodeStyleAttrMap a where
    toNodeStyleAttrMap :: a -> Map.Map Name T.Text