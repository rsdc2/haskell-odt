{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.ODT.ODT (
      getLastODT
    , removeLastODT
    , getStylesODT
    , getParaStylesODT
    , getTextStylesODT
    , nextParaStyleName
    , nextTextStyleName
    , paraStyleNameInts
    , textStyleNameInts
    , HasODT(..)
    , HasTextStyles(..)
    , IsNodes(..)
    , IsODT(..)
    , ODT(..)
    , OfficeNodeType(..)
    , TextNodeType(..)
    , TextLeafType(..)
) where

import Text.XML
    ( Document(..)
    , Element(..)
    , Node(..)
    , Name(..)
    , Prologue(..)
    , Miscellaneous(..) )

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.List as L

import Text.ODT.Utils.Types (
      IsText(..)
    , Stringable(..))

import Text.ODT.XML.Doc ( 
      Namespaces
    , Epilogue )

import Text.ODT.Style
import Text.ODT.ODTXML.Attr
import Text.ODT.ODTXML.Namespace
import Text.ODT.ODTXML.ODTXML
import Text.ODT.ODTXML.Name
import Text.ODT.XML.Types 

import Text.Read

type Id = T.Text
type Parent = Node

-- Types

data OfficeNodeType = 
      DocContent 
    | AutoStyles
    | DocStyles
    | FontFaceDecls
    | MasterStyles
    | Scripts
    | Styles
    | Style
    | Body
    | OfficeTextNode
    deriving Show

data StyleNodeType =
      DefStyle
    | GraphicProps
    | ParaProps
    | StyleType
    | TabStops
    | TableProps
    | TableRowProps
    | TextPropsNode
    deriving Show

data TextNodeType = 
      P (Maybe ParaStyle)
    | Span (Maybe TextStyle)
    | Note 
    | NoteBody 
    | SequenceDecls
    deriving Show

data TextLeafType =
      Str
    | NoteCit
    deriving Show

data ODT =
      OfficeNode    OfficeNodeType  ODTXML  ODT
    | TextNode      TextNodeType    ODTXML  ODT
    | TextLeaf      TextLeafType    ODTXML
    | StyleNode     StyleNodeType   ODTXML  ODT
    | ODTSeq        ODT             ODT 
    | MiscODT       ODTXML
    | EmptyODT 

instance Show ODT where
    show (TextLeaf    NoteCit   n     ) = show NoteCit
    show (TextLeaf    typ       n     ) = show typ <> " \"" <> T.unpack (getXMLText n) <> "\"" 
    show (TextNode    typ       _ odt ) = show typ <> " (" <> show odt <> ")"
    -- show (StyleNode  StyleType  _ odt )  = show StyleType <> " (" <> show odt <> ")"
    show (StyleNode  TextPropsNode n odt )  = show TextPropsNode <> " (" <> show odt <> ")"
    show (StyleNode  typ       _ odt )  = show typ <> " (" <> show odt <> ")"
    show (OfficeNode  typ       _ odt ) = show typ <> " (" <> show odt <> ")"
    show (MiscODT               n     ) = "MiscODT:" <> (T.unpack $ getLocalName n) 
    show (ODTSeq      odt1    odt2  )   = "ODTSeq " <> "(" <> show odt1 <> ", " <> show odt2 <> ")"
    show EmptyODT = "_"

-- Monoid instances
instance Semigroup ODT where
    (<>) :: ODT -> ODT -> ODT

    -- APPEND STYLES TO DOCUMENT
    OfficeNode AutoStyles  n1 odt1 <> StyleNode StyleType n2 odt2        
        | Nothing <- style = autostyles
        | Just (Left textstyle) <- style = case hasTextStyle textstyle autostyles of
            True -> autostyles  
            False -> OfficeNode AutoStyles n1 (odt1 <> styleODT)
        | Just (Right parastyle) <- style = case hasParaStyle parastyle autostyles of
            True -> autostyles  
            False -> OfficeNode AutoStyles n1 (odt1 <> styleODT)
            
        where autostyles = OfficeNode AutoStyles n1 odt1
              styleODT = StyleNode StyleType n2 odt2
              style = toStyle $ styleODT   

    OfficeNode  DocStyles  n1 odt1 <> StyleNode    StyleType   n2 odt2   = OfficeNode DocStyles n1 $ odt1 <> StyleNode    StyleType   n2 odt2
    
    -- TODO check for styles with the same name 
    OfficeNode  Styles  n1 odt1 <> StyleNode    StyleType   n2 odt2   = OfficeNode Styles n1 $ ODTSeq (StyleNode    StyleType   n2 odt2) odt1

    OfficeNode  officeType  n1 odt1 <> StyleNode    StyleType   n2 odt2             = OfficeNode officeType n1 (odt1 <> StyleNode    StyleType   n2' odt2) 
        where   styleODT = StyleNode    StyleType   n2 odt2
                nextStyleName = nextAutoStyleName styleODT odt1
                n2' = setAttrVal styleNameName nextStyleName n2

    StyleNode   StyleType   n1 odt1 <> StyleNode    StyleType   n2 odt2             = ODTSeq (StyleNode StyleType n1 odt1) (StyleNode StyleType n2 odt2)

    ODTSeq (OfficeNode Styles n1 odt1) (odt2) <> StyleNode StyleType n3 odt3        = ODTSeq (OfficeNode Styles n1 odt1 <> StyleNode StyleType n3 odt3) (odt2)
    ODTSeq (OfficeNode AutoStyles n1 odt1) (odt2) <> StyleNode StyleType n3 odt3    = ODTSeq (OfficeNode AutoStyles n1 odt1 <> StyleNode StyleType n3 odt3) (odt2)


    -- APPENDING / PREPENDING TEXT
    -- Feed elements of ODTSeq to DocContent individually
    OfficeNode  DocContent  n1 odt1 <> ODTSeq odt2 odt3                             = (OfficeNode DocContent n1 odt1 <> odt2) <> odt3 
    (ODTSeq odt1 odt2) <> OfficeNode  DocContent  n3 odt3                           = odt1 <> removeLastODT odt2 <> getLastODT odt2 <> OfficeNode DocContent n3 odt3

    -- Style carried with the Span is added to the document before adding the TextNode
    OfficeNode  DocContent  n1 odt1 <> TextNode (P (Just parastyle)) n2 odt2     = docContent' <> TextNode (P Nothing) n2' odt2
        where   parastyleodt = toODT parastyle
                docContent' = OfficeNode DocContent  n1 odt1 <> parastyleodt
                parastylename = getParaStyleName parastyle docContent'
                n2' = setAttrVal textStyleNameName parastylename n2 

    OfficeNode  DocContent  n1 odt1 <> TextNode (Span (Just textstyle)) n2 odt2     = docContent' <> TextNode (Span Nothing) n2' odt2
        where   textstyleodt = toODT textstyle
                docContent' = OfficeNode DocContent  n1 odt1 <> textstyleodt
                textstylename = getTextStyleName textstyle docContent'
                n2' = setAttrVal textStyleNameName textstylename n2 

    TextNode (Span (Just textstyle)) n2 odt2  <> OfficeNode  DocContent  n1 odt1   = (TextNode (Span Nothing) n2' odt2) <> docContent'
        where   textstyleodt = toODT textstyle
                docContent' = OfficeNode DocContent  n1 odt1 <> textstyleodt
                textstylename = getTextStyleName textstyle docContent'
                n2' = setAttrVal textStyleNameName textstylename n2 

    -- APPENDING / PREPENDING TEXT TO OFFICE NODES
    OfficeNode  OfficeTextNode    n1 odt1 <> TextNode     textType    n2 odt2       = OfficeNode OfficeTextNode n1 (odt1 <> TextNode textType n2 odt2)
    TextNode    textType          n2 odt2 <> OfficeNode   OfficeTextNode    n1 odt1 = OfficeNode OfficeTextNode n1 (TextNode textType n2 odt2 <> odt1)
    
    OfficeNode  officeType  n1 odt1 <> TextNode     textType    n2 odt2             = OfficeNode officeType n1 (odt1 <> TextNode textType n2 odt2)
    TextNode    textType    n2 odt2 <> OfficeNode   officeType  n1 odt1             = OfficeNode officeType n1 (TextNode textType n2 odt2 <> odt1)
    TextNode    textType    n1 odt1 <> ODTSeq   (OfficeNode OfficeTextNode n2 odt2) odt3 = ODTSeq   (OfficeNode OfficeTextNode n2 $ TextNode    textType    n1 odt1 <> odt2) odt3
    TextNode    textType    n1 odt1 <> ODTSeq   (OfficeNode officeType n2 odt2) odt3 = ODTSeq   (OfficeNode officeType n2 odt2) $ TextNode    textType    n1 odt1 <> odt3

    OfficeNode  officeType  n1 odt1 <> TextLeaf     textType    n2                  = OfficeNode officeType n1 (odt1 <> TextLeaf textType n2) 

    -- APPENDING / PREPENDING TEXT TO TEXT
    -- Insert new text after sequence-decls
    TextNode    textType    n1 odt1 <> TextNode     SequenceDecls    n2 odt2        = ODTSeq (TextNode SequenceDecls n2 odt2) (TextNode textType n1 odt1) 

    -- Appending to P nodes
    TextNode    (P ps1)     n1 odt1 <> TextNode (P ps2) n2 odt2             = ODTSeq (TextNode (P ps1) n1 odt1) (TextNode (P ps2) n2 odt2)
    -- Only adds Span into P if there is no style information to get from the document level
    TextNode    (P ps)      n1 odt1 <> TextNode     (Span Nothing)        n2 odt2   = TextNode (P ps) n1 (odt1 <> TextNode (Span Nothing) n2 odt2)
    -- If there is style information, wrap in ODTSeq so that waits until meets an element that can get style information from
    TextNode    (P ps)      n1 odt1 <> TextNode     (Span textstyle)        n2 odt2 = ODTSeq (TextNode (P ps) n1 odt1) (TextNode (Span textstyle) n2 odt2)
    TextNode    (P ps)      n1 odt1 <> TextLeaf     Str         n2                  = TextNode (P ps) n1 (odt1 <> TextLeaf Str n2)
    
    -- Appending to Span nodes
    TextNode    (Span textstyle)        n1 odt1 <> TextLeaf     Str         n       = ODTSeq (TextNode (Span textstyle) n1 odt1) (TextLeaf Str n) 
    TextLeaf    Str n1              <> TextLeaf     Str         n2                  = ODTSeq (TextLeaf Str n1) (TextLeaf Str n2)
    
    -- EMPTY NODES
    EmptyODT                        <> odt                                          = odt
    odt                             <> EmptyODT                                     = odt
    
    -- GENERAL RULES
    (ODTSeq odt1 odt2)                <> odt3                                         = ODTSeq odt1 (odt2 <> odt3)
    odt1                            <> TextNode     (P ps)           n odt2              = ODTSeq odt1 (TextNode (P ps) n odt2)
    odt1                            <> TextNode     (Span textstyle)   n odt2       = ODTSeq odt1 (TextNode (Span textstyle) n odt2)
    
    -- TODO: insert a pattern for OfficeNode Text <> TextNode and TextNode OfficeNode

    -- When an ODTSeq is appended to an ODT, append each element individually
    -- This is what allows style names etc. to be imparted to Span nodes before appending
    -- TODO test if this is still needed: in practice appending is done to the document
    odt1                            <> ODTSeq       odt2        odt3                = (odt1 <> odt2) <> odt3

    -- ERROR: NO PATTERN MATCH
    odt1                            <> odt2                                         = error ("No pattern for \n\t" <> show odt1 <> "\n\t" <> show odt2)

instance Monoid ODT where
    mempty :: ODT
    mempty = EmptyODT

-- classes based on ODT

class IsODT a where
    toODT :: a -> ODT

class HasODT a where
    getODT :: a -> ODT
    prependODT :: ODT -> a -> a
    appendODT :: ODT -> a -> a

-- IsODT instances

instance IsODT Node where
    toODT :: Node -> ODT 
    toODT n 
        | hasName       "automatic-styles"  officeNS  n = OfficeNode  AutoStyles  n' odt
        | hasName       "body"              officeNS  n = OfficeNode  Body        n' odt
        | hasName       "document-styles"   officeNS  n = OfficeNode  DocStyles n' odt
        | hasName       "font-face-decls"   officeNS  n = OfficeNode  FontFaceDecls n' odt
        | hasName       "master-styles"     officeNS  n = OfficeNode  MasterStyles      n' odt
        | hasName       "scripts"           officeNS  n = OfficeNode  Scripts      n' odt
        | hasName       "styles"            officeNS  n = OfficeNode  Styles      n' odt
        | hasName       "text"              officeNS  n = OfficeNode  OfficeTextNode          n' odt
        | hasName       "note"              textNS    n = TextNode    Note              n' odt
        | hasName       "note-body"         textNS    n = TextNode    NoteBody          n' odt
        | hasName       "note-citation"     textNS    n = TextLeaf    NoteCit           n'
        | hasLocalName  "NodeContent"                 n = TextLeaf    Str         n'
        | hasName       "p"                 textNS    n = TextNode    (P Nothing)          n' odt
        | hasName       "span"              textNS    n = TextNode    (Span Nothing)       n' odt
        | hasName       "sequence-decls"    textNS    n = TextNode   SequenceDecls       n' odt
        | hasName       "style"             styleNS   n = StyleNode  StyleType   n' odt
        | hasName       "text-properties"   styleNS   n = StyleNode  TextPropsNode   n' odt
        | otherwise                                     = MiscODT     (ODTXMLOrig $ n)
        where 
            n' = toODTXML n
            odt = fromNodes (getChildren n)

instance IsODT Element where
    toODT :: Element -> ODT
    toODT e 
        | hasName       "document-content"  officeNS  e = OfficeNode  DocContent  n' odt  
        | hasName       "document-styles"   officeNS  e = OfficeNode  DocStyles   n' odt  
        | otherwise                                     = MiscODT (ODTXMLOrig $ toNode e)
        where 
            n' = toODTXML e
            odt = fromNodes (getChildren e)


-- IsNodes instances

instance IsNodes ODT where
    fromNodes :: [Node] -> ODT
    fromNodes [] = EmptyODT 
    fromNodes (n:[]) = toODT n
    fromNodes (n:ns) = ODTSeq (toODT n) (fromNodes ns)

    toNodes :: ODT -> [Node]
    toNodes (OfficeNode  _ n' odt) = [appendChildren (toNodes odt) . toNode $ n']
    toNodes (StyleNode  _ n' odt) = [appendChildren (toNodes odt) . toNode $ n']
    toNodes (TextNode    _ n' odt) = [appendChildren (toNodes odt) . toNode $ n']
    toNodes (TextLeaf    _ n')     = [toNode n']
    toNodes (MiscODT       n')     = [toNode n']
    toNodes (ODTSeq odt1 odt2)     = toNodes odt1 <> toNodes odt2
    toNodes EmptyODT = []

instance HasAttrs ODT where 
    getAttrs :: ODT -> Map.Map Name T.Text
    getAttrs (OfficeNode    _  odtxml  _) = getAttrs odtxml
    getAttrs (TextNode    _  odtxml  _) = getAttrs odtxml
    getAttrs (TextNode _ odtxml _) = getAttrs odtxml
    getAttrs (TextLeaf      _    odtxml ) = getAttrs odtxml
    getAttrs (StyleNode     _   odtxml  _ ) = getAttrs odtxml
    getAttrs (ODTSeq odt1 odt2) = Map.unions [getAttrs odt1, getAttrs odt2]
    getAttrs (MiscODT       odtxml) = Map.empty
    getAttrs EmptyODT = Map.empty

    getAttrVal :: Name -> ODT -> T.Text
    getAttrVal name odt = fromMaybe "" . Map.lookup name . getAttrs $ odt

    hasAttrVal :: Name -> T.Text -> ODT -> Bool
    hasAttrVal name text odt = getAttrVal name odt == text

    setAttrVal :: Name -> T.Text -> ODT -> ODT
    setAttrVal attrname attrval odt = odt -- TODO needs proper treatment

-- Used to realise element attributes as type constructors
getTypeFromAttr :: (HasAttrs a, IsString b) => Name -> a -> b 
getTypeFromAttr name hasattrs = fromString . T.unpack . getAttrVal name $ hasattrs

instance HasTextStyles ODT where
    -- Returns a list of TextStyles from an ODT
    getTextStyles :: ODT -> [TextStyle]
    getTextStyles odt1
        | Just ts <- toTextStyle autostyles = [ts]
        | ODTSeq odt2 odt3 <- autostyles = getTextStyles odt2 <> getTextStyles odt3
        | EmptyODT <- autostyles = []
        | otherwise = []
        where   autostyles = getTextStylesODT odt1
                stylename = Just . getAttrVal styleNameName $ odt1

    getTextStyleName :: TextStyle -> ODT -> T.Text
    getTextStyleName ts1 odt1 
        | ODTSeq odt2 odt3 <- getTextStylesODT $ odt1 = case toTextStyle odt2 == Just ts1 of
            True -> getAttrVal styleNameName odt2
            False -> getTextStyleName ts1 odt3 
        | StyleNode StyleType n1 textprops <- getTextStylesODT odt1 = case toTextStyle (StyleNode StyleType n1 textprops) == Just ts1 of
            True -> getAttrVal styleNameName n1
            False -> error $ show odt1
        | EmptyODT <- getTextStylesODT odt1 = error $ show odt1
        | otherwise = error $ show odt1

    hasTextStyle :: TextStyle -> ODT -> Bool
    hasTextStyle ts1 odt1
        | ODTSeq odt2 odt3 <- getTextStylesODT $ odt1 = case toTextStyle odt2 == Just ts1 of
            True -> True
            False -> hasTextStyle ts1 odt3
        | StyleNode StyleType n1 textprops <- getTextStylesODT odt1 = toTextStyle (StyleNode StyleType n1 textprops) == Just ts1
        | EmptyODT <- getTextStylesODT odt1 = False
        | otherwise = False

instance HasParaStyles ODT where
    -- Returns a list of TextStyles from an ODT
    getParaStyles :: ODT -> [ParaStyle]
    getParaStyles odt1
        | Just ps <- toParaStyle autostyles = [ps]
        | ODTSeq odt2 odt3 <- autostyles = getParaStyles odt2 <> getParaStyles odt3
        | EmptyODT <- autostyles = []
        | otherwise = []
        where   autostyles = getParaStylesODT odt1
                stylename = Just . getAttrVal styleNameName $ odt1

    getParaStyleName :: ParaStyle -> ODT -> T.Text
    getParaStyleName ps1 odt1 
        | ODTSeq odt2 odt3 <- getParaStylesODT $ odt1 = case toParaStyle odt2 == Just ps1 of
            True -> getAttrVal styleNameName odt2
            False -> getParaStyleName ps1 odt3 
        | StyleNode StyleType n1 textprops <- getParaStylesODT odt1 = case toParaStyle (StyleNode StyleType n1 textprops) == Just ps1 of
            True -> getAttrVal styleNameName n1
            False -> error $ show odt1
        | EmptyODT <- getParaStylesODT odt1 = error $ show odt1
        | otherwise = error $ show odt1

    hasParaStyle :: ParaStyle -> ODT -> Bool
    hasParaStyle ps1 odt1
        | ODTSeq odt2 odt3 <- getParaStylesODT $ odt1 = case toParaStyle odt2 == Just ps1 of
            True -> True
            False -> hasParaStyle ps1 odt3
        | StyleNode StyleType n1 textprops <- getParaStylesODT odt1 = toParaStyle (StyleNode StyleType n1 textprops) == Just ps1
        | EmptyODT <- getParaStylesODT odt1 = False
        | otherwise = False


instance MaybeTextProps ODT where
  toTextProps :: ODT -> Maybe TextProps
  toTextProps (StyleNode TextPropsNode n _) = 
    Just TextProps {
      fontStyle = getTypeFromAttr foFsName n
    , fontWeight = getTypeFromAttr foFwName n 
    , underline = getTypeFromAttr (toName StyleNS "text-underline-style") n
    , textPosition = getTypeFromAttr (toName StyleNS "text-position") n
  }
  toTextProps _ = Nothing

instance MaybeParaStyle ODT where
  isParaStyle :: ODT -> Bool
  isParaStyle odt 
      | getAttrVal styleFamilyName odt == "paragraph" = True
      | otherwise = False


  toParaStyle :: ODT -> Maybe ParaStyle
  toParaStyle (StyleNode StyleType n1 odt2) = 
        case isParaStyle odt1 of
            True -> Just ParaStyle {
                          paraTextProps = textprops
                        , paraStyleName = Just . getAttrVal styleNameName $ odt1
                        , parentStyleName = getAttrVal parentStyleNameName $ odt1
                    }
            False -> Nothing

      where odt1 = StyleNode StyleType n1 odt2
            textpropsodt = getTextPropsODT odt2
            textpropsodt'   -- Include this stage to ensure that only one <text-properties> node is returned
              | ODTSeq x y <- textpropsodt = error "More than one <text-properties> node"
              | EmptyODT <- textpropsodt = error "No <text-properties> node"
              | StyleNode TextPropsNode n odt3 <- textpropsodt = textpropsodt
              | otherwise = error "Unknown error"
            textprops 
              | Just tp <- toTextProps textpropsodt = tp
              | Nothing <- toTextProps textpropsodt = newTextProps
              
  toParaStyle _ = Nothing

instance MaybeTextStyle ODT where
  isTextStyle :: ODT -> Bool
  isTextStyle odt 
      | getAttrVal styleFamilyName odt == "text" = True
      | otherwise = False

  toTextStyle :: ODT -> Maybe TextStyle
  toTextStyle odt1
      | StyleNode StyleType n1 (StyleNode TextPropsNode n2 _) <- odt1 = 
          case isTextStyle odt1 of
              True -> Just TextStyle {
                            textTextProps = TextProps {
                                fontStyle = getTypeFromAttr foFsName n2
                              , fontWeight = getTypeFromAttr foFwName n2 
                              , underline = getTypeFromAttr (toName StyleNS "text-underline-style") n2
                              , textPosition = getTypeFromAttr (toName StyleNS "text-position") n2
                            }
                          , textStyleName = Just . getAttrVal styleNameName $ odt1
                      }
              False -> Nothing
      | otherwise = Nothing

-- TODO: complete
instance MaybeStyle ODT where 

  getStyleFamily :: ODT -> Maybe StyleFamily
  getStyleFamily odt 
    | Nothing <- toTextStyle odt = case toParaStyle odt of
        Nothing -> Nothing
        Just parastyle -> Just ParaFamily
    | Just textstyle <- toTextStyle odt = Just TextFamily

  toStyle :: ODT -> Maybe (Either TextStyle ParaStyle)
  toStyle odt 
    | Nothing <- toTextStyle odt = case toParaStyle odt of
        Nothing -> Nothing
        Just parastyle -> Just (Right parastyle)
    | Just textstyle <- toTextStyle odt = Just (Left textstyle)

styleToODT :: (IsStyle a, HasTextProps a) => a -> ODT
styleToODT style = StyleNode StyleType odtxml odt
    where attrs = toStyleAttrMap style
          odtxml = ODTXMLElem (toName StyleNS "style") attrs
          odt = toTextPropsODT style

instance IsODT TextStyle where
  toODT ::   TextStyle   ->    ODT
  toODT = styleToODT

instance IsODT ParaStyle where
  toODT ::   ParaStyle   ->    ODT
  toODT = styleToODT

getLastODT :: ODT -> ODT
getLastODT (ODTSeq odt1 EmptyODT) = EmptyODT
getLastODT (ODTSeq odt1 odt2) = getLastODT odt2
getLastODT odt = odt

removeLastODT :: ODT -> ODT
removeLastODT (ODTSeq odt1 EmptyODT) = odt1
removeLastODT (ODTSeq odt1 odt2) = ODTSeq odt1 (removeLastODT odt2)
removeLastODT odt = EmptyODT

-- Returns an ODTSeq (or single instance) of styles
-- i.e. <style:text-properties>
getTextPropsODT :: ODT -> ODT
getTextPropsODT odt1
  | OfficeNode typ n odt2 <- odt1 = getTextPropsODT odt2
  | StyleNode TextPropsNode n odt2 <- odt1 = odt1
  | ODTSeq odt2 odt3 <- odt1 = getTextPropsODT odt2 <> getTextPropsODT odt3
  | otherwise = EmptyODT

-- Returns an ODTSeq (or single instance) of styles
-- i.e. <style:style>
-- Does not return the <office:styles> node
getStylesODT :: ODT -> ODT
getStylesODT odt1
  | OfficeNode typ n odt2 <- odt1 = getStylesODT odt2
  | StyleNode StyleType n odt2 <- odt1 = StyleNode StyleType n odt2
  | ODTSeq odt2 odt3 <- odt1 = getStylesODT odt2 <> getStylesODT odt3
  | otherwise = EmptyODT

-- Returns an ODTSeq (or single instance) of a style node of a specified family
getStyleODTByFamily :: StyleFamily -> ODT -> ODT
getStyleODTByFamily stylefamily odt1 
  | OfficeNode typ n odt2 <- odt1 = getStyleODTByFamily stylefamily odt2
  | StyleNode StyleType n odt2 <- odt1 = case getAttrVal styleFamilyName n of
      stylefamilyAttrVal -> StyleNode StyleType n odt2
      otherwise -> EmptyODT
  | ODTSeq odt2 odt3 <- odt1 = getStyleODTByFamily stylefamily odt2 <> getStyleODTByFamily stylefamily odt3
  | otherwise = EmptyODT
      where stylefamilyAttrVal = getAttrText stylefamily

-- Returns an ODTSeq (or single instance) of text styles
getTextStylesODT :: ODT -> ODT
getTextStylesODT = getStyleODTByFamily TextFamily

-- Returns an ODTSeq (or single instance) of paragraph styles
getParaStylesODT :: ODT -> ODT
getParaStylesODT = getStyleODTByFamily ParaFamily

-- TODO: implement Type Class containing the prefix value of each family
readAutoStyleNameAsInt :: StyleFamily -> [Char] -> Maybe Int
readAutoStyleNameAsInt family s 
  | [] <- s = Nothing
  | ParaFamily <- family = case take 1 s of
      "P" -> Just $ read . drop 1 $ s
      _ -> Nothing
  | TextFamily <- family = case take 1 s of
      "T" -> Just $ read . drop 1 $ s
      _ -> Nothing
  | otherwise = Nothing

-- Convert an automatic text style name to an integer, e.g. "T1" -> 1
readTextStyleNameAsInt :: [Char] -> Maybe Int
readTextStyleNameAsInt = readAutoStyleNameAsInt TextFamily

-- Convert an automatic paragraph style name to an integer, e.g. "P1" -> 1
readParaStyleNameAsInt :: [Char] -> Maybe Int
readParaStyleNameAsInt = readAutoStyleNameAsInt ParaFamily

-- Get the next text style name not already specified
-- TODO change to get maximum
nextTextStyleName :: ODT -> T.Text
nextTextStyleName odt = T.pack $ "T" <> (show . (+ 1) . maximum . textStyleNameInts $ odt)

-- Get the next paragraph style name not already specified
-- TODO: change to get maximum
nextParaStyleName :: ODT -> T.Text
nextParaStyleName odt = T.pack $ "P" <> (show . (+ 1) . maximum . paraStyleNameInts $ odt)

nextAutoStyleName :: ODT -> ODT -> T.Text
nextAutoStyleName styleODT odt = case getStyleFamily styleODT of
  Just TextFamily -> nextTextStyleName odt
  Just ParaFamily -> nextParaStyleName odt
  Just (MiscFamily _) -> error "No name for Misc style family"
  Nothing -> error $ "No style name for \n" <> show styleODT

-- Takes a sequence of ODTs containing styles, and returns the style name with the highest number
-- This is used for naming styles that don't appear in auto-styles
lastAutoStyleName :: StyleFamily -> ODT -> T.Text
lastAutoStyleName sf odt 
    | StyleNode StyleType n _ <- styles = getAttrVal styleNameName $ n
    | ODTSeq odt1 odt2 <- styles = lastTextStyleName odt2
    | EmptyODT <- styles = ""
    | otherwise = ""
    where styles = getStyleODTByFamily sf odt

lastTextStyleName :: ODT -> T.Text
lastTextStyleName = lastAutoStyleName TextFamily

lastParaStyleName :: ODT -> T.Text
lastParaStyleName = lastAutoStyleName ParaFamily

-- extracts all the int parts of TextStyle names
autoStyleNameInts :: StyleFamily -> ODT -> [Int]
autoStyleNameInts sf odt1 
  | StyleNode StyleType n _ <- styles = catMaybes [readAutoStyleNameAsInt sf . T.unpack . getAttrVal styleNameName $ n]
  | ODTSeq odt2 odt3 <- styles = autoStyleNameInts sf odt2 <> autoStyleNameInts sf odt3
  | EmptyODT <- styles = []
  | otherwise = []
  where styles = getStyleODTByFamily sf odt1
        -- s = 

textStyleNameInts :: ODT -> [Int]
textStyleNameInts = autoStyleNameInts TextFamily

paraStyleNameInts :: ODT -> [Int]
paraStyleNameInts = autoStyleNameInts ParaFamily

toTextPropsODT :: (HasTextProps a) => a -> ODT
toTextPropsODT style = StyleNode TextPropsNode odtxml EmptyODT
    where   attrs = getTextPropsAttrMap style
            odtxml = ODTXMLElem (toName StyleNS "text-properties") attrs
