{-# LANGUAGE OverloadedStrings #-}

module Text.ODT.XML.Types (
      HasAttrs(..)
    , HasCursor(..)
    , HasNodeChildren(..)
    , HasNumber(..)
    , HasXMLName(..)
    , HasXMLText(..)
    , IsAttrMap(..)
    , IsNodes(..)
    , IsElem(..)
    , IsNode(..)
    , IsXMLDoc(..)
    ) where

import Text.XML.Cursor

import Text.XML
    ( Element
    , Node(..)
    , Name(..)
    , Document
    )

import qualified Data.Text as T
import qualified Data.Map as Map

class HasAttrs a where
    getAttrs :: a -> Map.Map Name T.Text
    getAttrVal :: Name -> a -> T.Text
    hasAttrVal :: Name -> T.Text -> a -> Bool
    setAttrVal :: Name -> T.Text -> a -> a

class HasCursor a where
    cursor :: a -> Cursor
    create :: Cursor -> Maybe a

class HasNodeChildren a where
    getChildren :: a -> [Node]
    appendChildren :: [Node] -> a -> a 

class HasNumber a where
    numberStr :: a -> Maybe String
    numberInt :: a -> Maybe Int

class HasXMLName a where
    getLabel :: a -> T.Text
    getLocalName :: a -> T.Text
    hasLocalName :: T.Text -> a -> Bool
    hasName :: T.Text -> T.Text -> a -> Bool

class HasXMLText a where
    getXMLText :: a -> T.Text

class IsAttrMap a where
    toAttrMap :: a -> Map.Map Name T.Text

class IsElem a where
    toElem :: a -> Element
    fromElem :: Element -> a

class IsNode a where
    toNode :: a -> Node
    fromNode :: Node -> a

class IsNodes a where
    toNodes :: a -> [Node]
    fromNodes :: [Node] -> a

class IsXMLDoc a where 
    toXMLDoc :: a -> Document
    fromXMLDoc :: Document -> a
