{-# LANGUAGE OverloadedStrings #-}

module Text.ODT.Style.TextStyles (
      normal
    , italic
    , underlineTextStyle
    , bold
    , boldItalic
    , footnoteAnchor
) 

where

import Text.ODT.Style.Types

normal :: TextStyle
normal = newTextStyle

italic :: TextStyle
italic = newTextStyle {textTextProps = newTextProps {fontStyle = Italic}}

underlineTextStyle :: TextStyle
underlineTextStyle = newTextStyle {textTextProps = newTextProps {underline = Solid}}

bold :: TextStyle
bold = newTextStyle {textTextProps = newTextProps {fontWeight = Bold}}

-- TODO find out why fontSize is set to ""
boldItalic :: TextStyle
boldItalic = newTextStyle {textTextProps = newTextProps {fontStyle = Italic, fontWeight = Bold, fontSize = ""}}

footnoteAnchor :: TextStyle
footnoteAnchor = newTextStyle {textTextProps = newTextProps {textPosition = "super 58%"}}
