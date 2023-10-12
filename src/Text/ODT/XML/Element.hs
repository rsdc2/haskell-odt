{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Text.ODT.XML.Element (
      elemNoChildren
    , HasNodeChildren(..)
    , HasXMLName(..)
    , IsNode(..)
)

where

import Text.ODT.XML.Types ( 
      HasNodeChildren(..)
    , HasXMLText(..)
    , HasXMLName(..)
    , IsNode(..)
    , HasAttrs(..) )

import Data.Text as T
import Data.Map as Map
import Data.Maybe

import Text.XML ( Element(..), Node(..), Name(..) )

import Text.ODT.XML.Name ( HasXMLName(..) )
import Text.ODT.XML.Doc ( Namespaces )

instance HasNodeChildren Element where
    appendChildren :: [Node] -> Element -> Element
    appendChildren ns (Element name attrs _) = Element name attrs ns 

    getChildren :: Element -> [Node]
    getChildren (Element _ _ ns) = ns

instance HasAttrs Element where
    getAttrs :: Element -> Map.Map Name T.Text
    getAttrs (Element _ attrs _) = attrs
    getAttrs _ = Map.empty

    getAttrVal :: Name -> Element -> T.Text
    getAttrVal name elem = fromMaybe "" . Map.lookup name . getAttrs $ elem

    hasAttrVal :: Name -> T.Text -> Element -> Bool
    hasAttrVal name text elem = getAttrVal name elem == text

    setAttrVal :: Name -> T.Text -> Element -> Element
    setAttrVal attrname attrval elem
        | Element elemname attrs children <- elem = Element elemname (Map.insert attrname attrval attrs) children
        | otherwise = elem


instance HasXMLName Element where 
    getLabel :: Element -> T.Text
    getLabel (Element name _ _) = getLabel name

    getLocalName :: Element -> T.Text
    getLocalName (Element (Name n _ _) _ _) = n

    hasLocalName :: T.Text -> Element -> Bool
    hasLocalName n1 (Element (Name n2 _ _) _ _) = n1 == n2 

    hasName :: T.Text -> T.Text -> Element -> Bool
    hasName name1 namespace1 (Element name _ _) = hasName name1 namespace1 name

instance IsNode Element where
    toNode :: Element -> Node
    toNode e = NodeElement e

    fromNode :: Node -> Element
    fromNode (NodeElement e) = e

instance HasXMLText Element where
    getXMLText :: Element -> T.Text
    getXMLText (Element name attrs children) = ""-- T.concat $ getXMLText <$> children 

elemNoChildren :: Element -> Element
elemNoChildren (Element name attrs _) = Element name attrs []
