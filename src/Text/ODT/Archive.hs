{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.ODT.Archive (
    Archive(..)
  , HasODT(..)
  , HasContentODT(..)
) where

import Text.ODT.ODT
import Text.ODT.Doc
import Text.ODT.ODTXML.Name
import Text.ODT.ODTXML.ODTXML
import Text.ODT.Style
import Text.ODT.XML.Attrs

data Archive = Archive {contentDoc :: Doc, stylesDoc :: Doc}

instance HasContentODT Archive where
    getContentDocODT :: Archive -> ODT
    getContentDocODT (Archive content _) = odt $ content

    replaceContentDocODT :: ODT -> Archive -> Archive
    replaceContentDocODT content archive = archive { contentDoc = existingContentDoc {odt = content} } 
        where existingContentDoc = contentDoc $ archive

instance HasStylesODT Archive where
    getStylesDocODT :: Archive -> ODT
    getStylesDocODT (Archive _ styles) = odt $ styles

    replaceStylesDocODT :: ODT -> Archive -> Archive
    replaceStylesDocODT styles archive = archive { stylesDoc = existingStylesDoc {odt = styles} } 
        where existingStylesDoc = stylesDoc archive

    appendStyleODT :: ODT -> Archive -> Archive
    appendStyleODT style archive = replaceStylesDocODT stylesDocOdt archive
        where stylesDocOdt = odt . appendODT style . stylesDoc $ archive

    appendStyle :: (IsStyle a, IsODT a) => a -> Archive -> Archive
    appendStyle style archive = 
        replaceStylesDocODT stylesDocOdt archive
        where stylesDocOdt = odt . appendODT (toODT style) . stylesDoc $ archive

instance HasODT Archive where
    getODT :: Archive -> ODT 
    getODT (Archive contentdoc styledoc) = getODT contentdoc <> getODT styledoc 

    appendODT :: ODT -> Archive -> Archive
    appendODT (TextNode (Span (Just textstyle)) n odt) (Archive contentdoc stylesdoc) = 
        case hasTextStyle textstyle stylesdoc of
            True -> Archive (appendODT (TextNode (Span Nothing) n' odt) contentdoc) stylesdoc
            False -> Archive (appendODT (TextNode (Span (Just textstyle)) n odt) contentdoc) stylesdoc
        
        where textstylename = getTextStyleName textstyle stylesdoc 
              n' = setAttrVal textStyleNameName textstylename n       

    appendODT (TextNode (P (Just parastyle)) n odt) (Archive contentdoc stylesdoc) = 
        case hasParaStyle parastyle stylesdoc of
            True -> Archive (appendODT (TextNode (P Nothing) n' odt) contentdoc) stylesdoc
            False -> Archive (appendODT (TextNode (P (Just parastyle)) n odt) contentdoc) stylesdoc
        
        where parastylename = getParaStyleName parastyle stylesdoc 
              n' = setAttrVal textStyleNameName parastylename n       

    appendODT (ODTSeq odt1 odt2) archive  = appendODT odt2 . appendODT odt1 $ archive   

    appendODT odt (Archive contentdoc stylesdoc) = Archive (appendODT odt contentdoc) stylesdoc

    prependODT :: ODT -> Archive -> Archive
    prependODT (TextNode (Span (Just textstyle)) n odt) (Archive contentdoc stylesdoc) = 
        case hasTextStyle textstyle stylesdoc of
            True -> Archive (prependODT (TextNode (Span Nothing) n' odt) contentdoc) stylesdoc
            False -> Archive (prependODT (TextNode (Span (Just textstyle)) n odt) contentdoc) stylesdoc
        
        where textstylename = getTextStyleName textstyle stylesdoc 
              n' = setAttrVal textStyleNameName textstylename n       

    prependODT (ODTSeq odt1 odt2) archive = prependODT odt1 $ prependODT (removeLastODT odt2) $ prependODT (getLastODT odt2) $ archive  

    prependODT odt  (Archive contentdoc stylesdoc)  = Archive (prependODT odt contentdoc) stylesdoc

