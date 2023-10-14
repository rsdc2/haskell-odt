{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.ODT.Style (
      FontSize(..)
    , FontStyle(..)
    , FontWeight(..)
    , HasParaStyles(..)
    , HasStyles(..)
    , HasTextProps(..)
    , HasTextStyles(..)
    , HasXMLText(..)
    , IsAttrText(..)
    , IsString(..) 
    , IsStyle(..)
    , MaybeParaStyle(..)
    , MaybeStyle(..)
    , MaybeTextProps(..)
    , MaybeTextStyle(..)
    , newParaStyle
    , newTextProps
    , newTextStyle
    , ParaStyle(..)
    , StyleFamily(..)
    , TextProps(..)
    , TextStyle(..)
    , Underline(..)
) where

import Data.String ( IsString(..) )
import Data.Maybe
import Data.Map as Map
import qualified Data.Text as T
import Text.XML ( Name(..) )

import Text.ODT.Utils.Types
import Text.ODT.XML.Types ( HasXMLText(..), IsAttrMap(..) )
import Text.ODT.ODTXML.Namespace
import Text.ODT.ODTXML.Attr
import Text.ODT.ODTXML.Name


class IsAttrText a where
  toAttrText :: a -> T.Text
  fromAttrText :: T.Text -> a

data FontSize =
    FontSize T.Text
  | NormalSize
  deriving (Show, Eq)

data FontStyle =
    NormalStyle
  | Italic
  deriving (Show, Eq)

data FontWeight =
    NormalWeight
  | Bold
  deriving (Show, Eq)

data Underline =
    Solid 
  | NoUnderline
  deriving (Show, Eq)

data TextPosition =
    TextPosition T.Text
  | NormalPosition
  deriving (Show, Eq)

data TextProps = TextProps {
    fontSize :: FontSize
  , fontStyle :: FontStyle
  , fontWeight :: FontWeight
  , underline :: Underline
  , textPosition :: TextPosition
}

data TextStyle = 
  TextStyle {
      textStyleName :: Maybe T.Text
    , textTextProps :: TextProps
  }

data ParaStyle = 
  ParaStyle {
      parentStyleName :: T.Text
    , paraStyleName :: Maybe T.Text
    , paraTextProps :: TextProps
  }

class HasTextProps a where
  getFontSize :: a -> FontSize
  getFontStyle :: a -> FontStyle
  getFontWeight :: a -> FontWeight
  getUnderline :: a -> Underline
  getTextPosition :: a -> TextPosition
  getTextPropsAttrMap :: a -> Map.Map Name T.Text

class IsStyle a where
  toStyleAttrMap :: a -> Map.Map Name T.Text

instance HasTextProps TextProps where
  getFontSize = fontSize
  getFontStyle = fontStyle
  getFontWeight = fontWeight
  getUnderline = underline
  getTextPosition = textPosition

  getTextPropsAttrMap :: TextProps -> Map.Map Name T.Text
  getTextPropsAttrMap textprops = Map.unions [  
      toTextPropAttrMap . getFontSize $ textprops
    , toTextPropAttrMap . getFontStyle $ textprops
    , toTextPropAttrMap . getFontWeight $ textprops  
    , toTextPropAttrMap . getUnderline $ textprops
    , toTextPropAttrMap . getTextPosition $ textprops  ]

instance HasTextProps TextStyle where
  getFontSize = fontSize . textTextProps
  getFontStyle = fontStyle . textTextProps 
  getFontWeight = fontWeight . textTextProps
  getUnderline = underline . textTextProps
  getTextPosition = textPosition . textTextProps
  getTextPropsAttrMap = getTextPropsAttrMap . textTextProps

instance HasTextProps ParaStyle where
  getFontSize = fontSize . paraTextProps
  getFontStyle = fontStyle . paraTextProps
  getFontWeight = fontWeight . paraTextProps
  getUnderline = underline . paraTextProps
  getTextPosition = textPosition . paraTextProps
  getTextPropsAttrMap = getTextPropsAttrMap . paraTextProps


newTextProps :: TextProps 
newTextProps = TextProps {
    fontSize = NormalSize
  , fontStyle = NormalStyle
  , fontWeight = NormalWeight
  , underline = NoUnderline
  , textPosition = NormalPosition
}

newTextStyle :: TextStyle
newTextStyle = TextStyle {    
    textTextProps = newTextProps
  , textStyleName = Nothing       
  }

newParaStyle :: ParaStyle
newParaStyle = ParaStyle {    
    paraTextProps = newTextProps
  , paraStyleName = Nothing       
  , parentStyleName = "Standard"     
  }

-- TODO add fontsize
instance Show TextStyle where
  show :: TextStyle -> String
  show ts = "TextStyle " <> "[" <> show fontsize <> "," <> show fs <> "," <> show fw <> "," <> show u <> "," <> show tp <> "," <> show name <> "]"
      where fontsize = getFontSize ts
            fs = getFontStyle ts
            fw = getFontWeight ts
            u = getUnderline ts
            tp = getTextPosition ts
            name = textStyleName ts

instance Show ParaStyle where
  show :: ParaStyle -> String
  show ts = "ParaStyle " <> "[" <> show fs <> "," <> show fw <> "," <> show u <> "," <> show tp <> "," <> show name <> "]"
      where fs = getFontStyle ts
            fw = getFontWeight ts
            u = getUnderline ts
            tp = getTextPosition ts
            name = paraStyleName ts

instance Eq TextStyle where
  x == y =       getFontSize x == getFontSize y
              && getFontStyle x == getFontStyle y 
              && getFontWeight x == getFontWeight y
              && getUnderline x == getUnderline y
              && getTextPosition x == getTextPosition y
    
instance Eq ParaStyle where
  x == y =       getFontSize x == getFontSize y
              && getFontStyle x == getFontStyle y 
              && getFontWeight x == getFontWeight y
              && getUnderline x == getUnderline y
              && getTextPosition x == getTextPosition y
              && parentStyleName x == parentStyleName y

data StyleFamily = 
    MiscFamily T.Text
  | ParaFamily
  | TextFamily

instance IsAttrText StyleFamily where   
  toAttrText :: StyleFamily -> T.Text
  toAttrText ParaFamily = "paragraph"
  toAttrText TextFamily = "text"
  toAttrText (MiscFamily txt) = txt -- not sure if this is correct

  fromAttrText :: T.Text -> StyleFamily
  fromAttrText s = case s of
    "paragraph" -> ParaFamily
    "text"      -> TextFamily 
    otherwise   -> MiscFamily s    

instance IsStyle StyleFamily where
  toStyleAttrMap :: StyleFamily -> Map.Map Name T.Text
  toStyleAttrMap sf = Map.fromList [(toName StyleNS "family", toAttrText sf)]

instance IsAttrText FontSize where
  toAttrText :: FontSize -> T.Text
  toAttrText (FontSize txt) = txt
  toAttrText NormalSize = ""  -- Not sure about this

  fromAttrText :: T.Text -> FontSize
  fromAttrText s
    | s == "" = NormalSize
    | otherwise = FontSize s   -- TODO make this check that format of string correct

instance IsString FontSize where 
  fromString :: String -> FontSize
  fromString = fromAttrText . T.pack

instance IsAttrText FontStyle where
  toAttrText :: FontStyle -> T.Text
  toAttrText Italic = "italic"
  toAttrText NormalStyle = "normal"

  fromAttrText :: T.Text -> FontStyle
  fromAttrText s
      | s == "italic" = Italic
      | otherwise = NormalStyle -- this is how it returns a normal style if there is no attribute 

instance IsAttrText FontWeight where
  toAttrText :: FontWeight -> T.Text
  toAttrText Bold = "bold"
  toAttrText NormalWeight = "normal"

  fromAttrText :: T.Text -> FontWeight
  fromAttrText s
      | s == "bold" = Bold
      | otherwise = NormalWeight -- this is how it returns a normal weight if there is no attribute 

instance IsAttrText Underline where
  toAttrText :: Underline -> T.Text
  toAttrText Solid = "solid"
  toAttrText NoUnderline = "none"

  fromAttrText :: T.Text -> Underline
  fromAttrText s 
      | s == "solid" = Solid
      | s == "none" = NoUnderline
      | otherwise = NoUnderline -- TODO replace with text so that preserves underlines that does not understand

instance IsAttrText TextPosition where
  toAttrText :: TextPosition -> T.Text
  toAttrText (TextPosition txt) = txt
  toAttrText NormalPosition = "" -- TODO work out what this should be

  fromAttrText :: T.Text -> TextPosition
  fromAttrText s 
      | s == "" = NormalPosition
      | otherwise = TextPosition s

instance IsString TextPosition where
  fromString :: String -> TextPosition
  fromString = fromAttrText . T.pack

-- Used in the creation of a new style node
instance IsTextPropAttrMap FontSize where
  toTextPropAttrMap :: FontSize -> Map.Map Name T.Text
  toTextPropAttrMap NormalSize = Map.empty
  toTextPropAttrMap fs = Map.fromList   [   
      (toName StyleNS "font-size-asian",    toAttrText fs)
    , (toName StyleNS "font-size-complex",  toAttrText fs)
    , (toName FoNS    "font-size",          toAttrText fs) 
    ]

instance IsTextPropAttrMap FontStyle where
  toTextPropAttrMap :: FontStyle -> Map.Map Name T.Text
  toTextPropAttrMap NormalStyle = Map.empty
  toTextPropAttrMap fs = Map.fromList   [   
      (toName StyleNS "font-style-asian",    toAttrText fs)
    , (toName StyleNS "font-style-complex",  toAttrText fs)
    , (toName FoNS    "font-style",          toAttrText fs) 
    ]

instance IsTextPropAttrMap FontWeight where
  toTextPropAttrMap :: FontWeight -> Map.Map Name T.Text
  toTextPropAttrMap NormalWeight = Map.empty
  toTextPropAttrMap fw = Map.fromList   [   
      (toName StyleNS "font-weight-asian",    toAttrText fw)
    , (toName StyleNS "font-weight-complex",  toAttrText fw)
    , (toName FoNS "font-weight",             toAttrText fw) ]

instance IsTextPropAttrMap Underline where
  toTextPropAttrMap :: Underline -> Map.Map Name T.Text
  toTextPropAttrMap NoUnderline = Map.empty
  toTextPropAttrMap u = Map.fromList   [    
      (toName StyleNS "text-underline-color", "font-color")
    , (toName StyleNS "text-underline-width", "auto")
    , (toName StyleNS "text-underline-style", toAttrText u) ]

instance IsTextPropAttrMap TextPosition where
  toTextPropAttrMap :: TextPosition -> Map.Map Name T.Text
  toTextPropAttrMap NormalPosition = Map.empty
  toTextPropAttrMap (TextPosition txt) = Map.fromList [(toName StyleNS "text-position", txt)]

-- Used when creating a new style node
instance IsStyle TextStyle where
  toStyleAttrMap :: TextStyle -> Map.Map Name T.Text
  toStyleAttrMap ts = Map.union (toStyleAttrMap TextFamily) (Map.fromList [(toName StyleNS "name", fromMaybe "" $ textStyleName ts)]) 

instance IsStyle ParaStyle where
  toStyleAttrMap :: ParaStyle -> Map.Map Name T.Text
  toStyleAttrMap ps = Map.union (toStyleAttrMap ParaFamily) (Map.fromList [ (toName StyleNS "name", fromMaybe "" $ paraStyleName ps),
                                                                            (toName StyleNS "parent-style-name", parentStyleName ps)])

-- Used when assigning text to a particular style
instance IsNodeStyleAttrMap TextStyle where
    toNodeStyleAttrMap :: TextStyle -> Map.Map Name T.Text
    toNodeStyleAttrMap ts
        | Nothing <- textStyleName ts = Map.empty
        | Just name <- textStyleName ts = Map.fromList [ (toName TextNS "style-name", name) ]

instance IsNodeStyleAttrMap ParaStyle where
    toNodeStyleAttrMap :: ParaStyle -> Map.Map Name T.Text
    toNodeStyleAttrMap ps
        | Nothing <- paraStyleName ps = Map.empty
        | Just name <- paraStyleName ps = Map.fromList [ (toName TextNS "style-name", name) ]

-- For generating style attributes in a style definition
class HasTextStyles a where
  getTextStyles :: a -> [TextStyle]
  getTextStyleName :: TextStyle -> a -> T.Text
  hasTextStyle :: TextStyle -> a ->  Bool

class HasParaStyles a where
  getParaStyles :: a -> [ParaStyle]
  getParaStyleName :: ParaStyle -> a -> T.Text
  hasParaStyle :: ParaStyle -> a ->  Bool

class HasStyles a where
  hasStyle :: StyleFamily -> a -> Bool

class MaybeTextProps a where
  toTextProps :: a -> Maybe TextProps

class MaybeTextStyle a where
  toTextStyle :: a -> Maybe TextStyle
  isTextStyle :: a -> Bool

class MaybeParaStyle a where
  toParaStyle :: a -> Maybe ParaStyle
  isParaStyle :: a -> Bool



class MaybeStyle a where
  -- isStyle :: a -> Bool
  -- isStyleFamily :: StyleFamily -> a -> Bool
  getStyleFamily :: a -> Maybe StyleFamily
  toStyle :: a -> Maybe (Either TextStyle ParaStyle)