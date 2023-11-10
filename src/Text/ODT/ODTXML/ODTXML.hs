{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Text.ODT.ODTXML.ODTXML (
    HasXMLText
  , IsNode(..)
  , HasAttrs(..)
  , HasXMLName(..)
  , ODTXML(..)
  , IsODTXML(..)
)

where

import Text.XML (Name(..), Node(..), Element(..))
import Data.Text as T
import qualified Data.Map as Map

import Text.ODT.XML.Node (nodeName)
import Text.ODT.XML.Types (
      IsNode(..)
    , HasAttrs(..)
    , HasXMLName(..)
    , HasXMLText(..) )

import Text.ODT.ODTXML.Namespace (
      officeNS
    , textNS )

import Data.Maybe

data ODTXML = 
      ODTXMLElem Name (Map.Map Name Text)
    | ODTXMLText T.Text
    | ODTXMLOrig Node
    deriving Eq

class IsODTXML a where
    toODTXML :: a -> ODTXML

instance HasAttrs ODTXML where
    getAttrs :: ODTXML -> Map.Map Name T.Text
    getAttrs (ODTXMLElem _ attrs) = attrs
    getAttrs _ = Map.empty

    getAttrVal :: Name -> ODTXML -> T.Text
    getAttrVal name odtxml = fromMaybe "" . Map.lookup name . getAttrs $ odtxml

    hasAttrVal :: Name -> T.Text -> ODTXML -> Bool
    hasAttrVal name text odtxml = getAttrVal name odtxml == text

    setAttrVal :: Name -> T.Text -> ODTXML -> ODTXML
    setAttrVal attrname attrval odtxml
        | ODTXMLElem elemname attrs <- odtxml = ODTXMLElem elemname (Map.insert attrname attrval attrs)
        | otherwise = odtxml

instance IsNode ODTXML where 
    toNode :: ODTXML -> Node
    toNode (ODTXMLElem name attrs) = NodeElement $ Element name attrs []
    toNode (ODTXMLText txt) = NodeContent txt
    toNode (ODTXMLOrig n) = n

    fromNode :: Node -> ODTXML
    fromNode (NodeElement (Element name attrs children)) = ODTXMLElem name attrs
    fromNode (NodeContent txt) = ODTXMLText txt
    fromNode n = ODTXMLOrig n

instance HasXMLText ODTXML where
    getXMLText :: ODTXML -> T.Text
    getXMLText (ODTXMLElem name attrs) = ""
    getXMLText (ODTXMLText txt) = txt
    getXMLText (ODTXMLOrig n) = getXMLText n

instance IsODTXML Node where
    toODTXML :: Node -> ODTXML
    toODTXML n = fromNode n

instance IsODTXML Element where
    toODTXML :: Element -> ODTXML
    toODTXML (Element name attrs children) = ODTXMLElem name attrs 

instance HasXMLName ODTXML where
    getLocalName :: ODTXML -> T.Text
    getLocalName (ODTXMLElem (Name n _ _) _) = n
    getLocalName (ODTXMLText _) = "NodeContent"
    getLocalName (ODTXMLOrig node) = nodeName node

    hasLocalName :: T.Text -> ODTXML -> Bool
    hasLocalName n1 (ODTXMLElem (Name n2 _ _) _) = n1 == n2
    hasLocalName n1 (ODTXMLText _) = "NodeContent" == n1
    hasLocalName n1 (ODTXMLOrig node) = getLocalName node == n1

    hasName :: T.Text -> T.Text -> ODTXML -> Bool
    hasName name1 namespace1 (ODTXMLElem name _) = hasName name1 namespace1 name
    hasName name1 namespace1 (ODTXMLText _) = "NodeContent" == name1
    hasName name namespace (ODTXMLOrig node) = hasName name namespace node

    getLabel :: ODTXML -> T.Text
    getLabel (ODTXMLElem name _) = getLabel name
    getLabel (ODTXMLText _) = ""
    getLabel (ODTXMLOrig node) = getLabel node

