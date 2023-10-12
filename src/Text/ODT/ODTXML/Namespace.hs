{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Text.ODT.ODTXML.Namespace (
      HasXMLName(..)
    , HasXMLText(..)
    , Namespace(..)
    , officeNS
    , textNS
    , styleNS
    , foNS )

where

import Text.XML (Name(..))
import Data.Text as T
import Data.String (IsString(..))

import Text.ODT.XML.Types (HasXMLText(..), HasXMLName(..))

officeNS = T.pack "urn:oasis:names:tc:opendocument:xmlns:office:1.0"
textNS = T.pack "urn:oasis:names:tc:opendocument:xmlns:text:1.0"
styleNS = T.pack "urn:oasis:names:tc:opendocument:xmlns:style:1.0"
foNS = T.pack "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0"

data Namespace = 
      OfficeNS
    | TextNS
    | StyleNS
    | FoNS

instance IsString Namespace where 
    fromString :: String -> Namespace 
    fromString s 
        | s == T.unpack officeNS = OfficeNS
        | s == T.unpack textNS = TextNS
        | s == T.unpack styleNS = StyleNS
        | s == T.unpack foNS = FoNS
        | otherwise = error $ "Cannot convert to Namespace from string " <> s 

-- TODO: use a different type class here; keep XML Text for XML text nodes
instance HasXMLText Namespace where
    getXMLText :: Namespace -> T.Text
    getXMLText OfficeNS = officeNS
    getXMLText TextNS = textNS
    getXMLText StyleNS = styleNS
    getXMLText FoNS = foNS

instance HasXMLName Namespace where 
    getLocalName :: Namespace -> T.Text
    getLocalName _ = ""

    hasLocalName :: T.Text -> a -> Bool
    hasLocalName _ _ = False

    hasName :: T.Text -> T.Text -> Namespace -> Bool
    hasName _ _ _ = False

    getLabel :: Namespace -> T.Text
    getLabel OfficeNS = "office"
    getLabel TextNS = "text"
    getLabel StyleNS = "style"
    getLabel FoNS = "fo"