{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.ODT.Doc (
    insertNewODT
  , Doc(..)
  , HasAttrs(..)
  , IsXMLDoc(..)
  , odtFromXMLDoc
  , odtFromODTDoc
) where

import Text.XML
    ( Document(..)
    , Element(..)
    , Node(..)
    , Name(..)
    , Prologue(..)
    , Miscellaneous(..) )

import Data.Maybe
import Text.Read

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.List as L

import Text.ODT.Utils.Types (
      IsText(..)
    , Stringable(..))

import Text.ODT.XML.Doc ( 
      Namespaces
    , Epilogue )

import Text.ODT.XML.Types 
import Text.ODT.ODT
import Text.ODT.Style
import Text.ODT.ODTXML.Attr
import Text.ODT.ODTXML.Namespace
import Text.ODT.ODTXML.ODTXML
import Text.ODT.ODTXML.Name


data Doc = 
    Doc {prologue :: Prologue, epilogue :: Epilogue, odt :: ODT}

-- instances

instance Show Doc where
    show (Doc _ _ odt) = "Doc " <> "(" <> (show odt) <> ")" 

instance HasAttrs Doc where
    getAttrs :: Doc -> Map.Map Name T.Text
    getAttrs = getAttrs . getODT

    getAttrVal :: Name -> Doc -> T.Text
    getAttrVal name = fromMaybe "" . Map.lookup name . getAttrs

    hasAttrVal :: Name -> T.Text -> Doc -> Bool
    hasAttrVal name text doc = getAttrVal name doc == text

    setAttrVal :: Name -> T.Text -> Doc -> Doc
    setAttrVal attrname attrval doc = doc -- placeholder; TODO needs proper treatment

instance HasODT Doc where
    getODT :: Doc -> ODT
    getODT (Doc _ _ odt) = odt

    prependODT :: ODT -> Doc -> Doc
    prependODT odt1 (Doc prlg eplg odt2) = Doc prlg eplg $ odt1 <> odt2

    appendODT :: ODT -> Doc -> Doc
    appendODT odt1 (Doc prlg eplg odt2) = Doc prlg eplg $ odt2 <> odt1

instance IsXMLDoc Doc where 
    -- TODO: other combinations
    toXMLDoc :: Doc -> Document
    -- TODO: find out why default name for DocContent is "body"
    -- TODO: find out why office version not included 
    toXMLDoc (Doc prlg eplg (OfficeNode DocContent (ODTXMLElem name attrs) odt)) = 
        Document prlg (Element (toName OfficeNS "document-content") attrs $ toNodes odt) eplg
        -- error $ show attrs
    toXMLDoc (Doc prlg eplg (OfficeNode DocStyles (ODTXMLElem name attrs) odt)) = 
        Document prlg (Element name attrs $ toNodes odt) eplg
    toXMLDoc doc = error $ show doc 
    -- toXMLDocument (Doc prlg eplg (OfficeNode typ odtxml odt)) = Document prlg () eplg

    fromXMLDoc :: Document -> Doc
    fromXMLDoc (Document prlg elem eplg) = 
        -- error $ show elem
        Doc prlg eplg $ toODT elem 

instance HasTextStyles Doc where
    getTextStyles :: Doc -> [TextStyle]
    getTextStyles doc = getTextStyles . getODT $ doc

    getTextStyleName :: TextStyle -> Doc -> T.Text
    getTextStyleName textstyle doc = getTextStyleName textstyle $ getODT doc

    hasTextStyle :: TextStyle -> Doc -> Bool
    hasTextStyle textstyle doc = hasTextStyle textstyle $ getODT doc

instance HasParaStyles Doc where
    getParaStyles :: Doc -> [ParaStyle]
    getParaStyles doc = getParaStyles . getODT $ doc

    getParaStyleName :: ParaStyle -> Doc -> T.Text
    getParaStyleName parastyle doc = getParaStyleName parastyle $ getODT doc

    hasParaStyle :: ParaStyle -> Doc -> Bool
    hasParaStyle parastyle doc = hasParaStyle parastyle $ getODT doc

insertNewODT :: ODT -> Doc -> Doc
insertNewODT odt (Doc prlg eplg _) = Doc prlg eplg odt

odtFromODTDoc :: Doc -> ODT
odtFromODTDoc (Doc _ _ odt) = odt

odtFromXMLDoc :: Document -> ODT
odtFromXMLDoc xmldoc = odtFromODTDoc . fromXMLDoc $ xmldoc
