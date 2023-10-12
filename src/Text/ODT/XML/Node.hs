{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Text.ODT.XML.Node (
      IsElem(..)
    , HasNodeChildren(..)
    , HasXMLText(..)
    , HasXMLName(..)
    , IsNode(..)
    , nodeName
)

where

import Text.XML (
      Node(..)
    , Element(..)
    , Name(..) )

import Text.ODT.XML.Types (
      HasNodeChildren(..)
    , HasXMLName(..)
    , IsElem(..)
    , IsNode(..)
    , HasXMLText(..)
    , HasAttrs(..))

import Text.ODT.XML.Element (HasXMLName(..))

import Data.Maybe

import qualified Data.Text as T
import qualified Data.Map as Map

type Children = [Node]

instance HasNodeChildren Node where
    appendChildren :: [Node] -> Node -> Node
    appendChildren ns (NodeElement (Element name attrs _)) = NodeElement $ Element name attrs ns 

    getChildren :: Node -> [Node]
    getChildren (NodeElement (Element _ _ children)) = children

instance HasAttrs Node where
    getAttrs :: Node -> Map.Map Name T.Text
    getAttrs (NodeElement (Element _ attrs _)) = attrs
    getAttrs _ = Map.empty

    getAttrVal :: Name -> Node -> T.Text
    getAttrVal name node = fromMaybe "" . Map.lookup name . getAttrs $ node

    hasAttrVal :: Name -> T.Text -> Node -> Bool
    hasAttrVal name text node = getAttrVal name node == text

    setAttrVal :: Name -> T.Text -> Node -> Node
    setAttrVal attrname attrval node
        | NodeElement (Element elemname attrs children) <- node = NodeElement (Element elemname (Map.insert attrname attrval attrs) children)
        | otherwise = node

instance HasXMLName Node where
    getLabel :: Node -> T.Text
    getLabel (NodeElement e) = getLabel e
    getLabel _ = ""

    getLocalName :: Node -> T.Text
    getLocalName (NodeElement (Element (Name n _ _) attrs _)) = n
    getLocalName _ = ""

    hasLocalName :: T.Text -> Node -> Bool
    hasLocalName n1 (NodeElement (Element (Name n2 _ _) _ _)) = n1 == n2
    hasLocalName n1 (NodeContent txt) = n1 == "NodeContent"
    hasLocalName _ _ = False

    hasName :: T.Text -> T.Text -> Node -> Bool
    hasName name1 namespace1 (NodeElement elem) = hasName name1 namespace1 elem
    hasName name namespace (NodeContent txt) = name == "NodeContent"
    hasName _ _ _ = False

instance IsNode Node where
    toNode :: Node -> Node
    toNode n = n

    fromNode :: Node -> Node
    fromNode n = n

instance HasXMLText Node where
    getXMLText :: Node -> T.Text
    getXMLText (NodeElement e) = getXMLText e
    getXMLText (NodeContent txt) = txt
    getXMLText (NodeComment txt) = txt
    getXMLText (NodeInstruction _) = ""

-- TODO: integrate with HasXMLName
nodeName :: Node -> T.Text
nodeName (NodeElement (Element (Name n _ _) _ _)) = n
nodeName (NodeContent _) = "NodeContent"
nodeName (NodeComment _) = "NodeComment"
nodeName (NodeInstruction _) = "NodeInstruction"
