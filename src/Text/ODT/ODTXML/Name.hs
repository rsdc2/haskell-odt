{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Text.ODT.ODTXML.Name (
      foFsName
    , foFwName
    , parentStyleNameName
    , pName
    , spanName
    , styleFamilyName
    , styleNameName
    , textStyleNameName
    , toName
)

where

import Text.XML ( Name(..) )
import Data.Text as T
import Text.ODT.ODTXML.Namespace

toName :: Namespace -> T.Text -> Name 
toName ns name = Name name (Just . getXMLText $ ns) (Just . getLabel $ ns)

pName :: Name
pName = toName TextNS "p"

spanName :: Name
spanName = toName TextNS "span"

foFwName :: Name
foFwName = toName FoNS "font-weight"

foFsName :: Name
foFsName = toName FoNS "font-style"

parentStyleNameName :: Name
parentStyleNameName = toName StyleNS "parent-style-name"

styleNameName :: Name
styleNameName = toName StyleNS "name"   -- The XML name of the the @style:name attribute

styleFamilyName :: Name
styleFamilyName = toName StyleNS "family"

textpropsName :: Name
textpropsName = toName StyleNS "text-properties"

-- e.g. <text:span text:style-name="T4">Some new bold text. </text:span>
textStyleNameName :: Name
textStyleNameName = toName TextNS "style-name"