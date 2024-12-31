{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Text.XML as XML

import Text.ODT.Diagnostics.Utils ( unzipOdt, prettifyOdt )

import Text.ODT.File ( path )
import Text.ODT.XML.Prettify ( prettifyFile )
import qualified Text.ODT.Zip.Zip as Zip 
import Text.ODT.ODT ( IsList(toList), HasODT(getODT), ODT )
import Text.ODT.Doc ( Doc(odt), IsXMLDoc(fromXMLDoc, toXMLDoc) )
import Text.ODT.Query ( getLastPara, getText, paraCount )
import qualified Text.ODT.Ops as ODT
import Text.ODT.Style
    ( ParaStyle(paraStyleName, paraTextProps),
      newTextProps,
      TextProps(fontStyle),
      FontStyle(Italic),
      newParaStyle )
import qualified Text.ODT.Style.TextStyles as TextStyles

exampleFileName :: String
exampleFileName = "example2"

-- Write a content doc and a styles doc to the designated path
writeODT :: Doc -> Doc -> IO ()
writeODT contentdoc stylesdoc = do

    let contentxmldoc = toXMLDoc contentdoc
    let stylesxmldoc = toXMLDoc stylesdoc

    -- Write modified documents back to file
    XML.writeFile XML.def (path "content3.xml") contentxmldoc
    XML.writeFile XML.def (path $ exampleFileName <> "/content.xml") contentxmldoc
    XML.writeFile XML.def (path $ exampleFileName <> "/styles.xml") stylesxmldoc
    XML.writeFile XML.def (path "styles3.xml") stylesxmldoc

    -- Produce a prettified version of the files
    prettifyFile (path "content1.xml") (path "content2.xml")
    prettifyFile (path "content3.xml") (path "content4.xml")
    prettifyFile (path "styles3.xml") (path "styles4.xml")

    -- Zip modified files
    Zip.zipODT (path $ exampleFileName <> ".odt") [path $ exampleFileName <> "/content.xml", path $ exampleFileName <> "/styles.xml"] (path "modified.odt")
    Zip.zipODT (path $ exampleFileName <> ".odt") [path $ exampleFileName <> "/content.xml", path $ exampleFileName <> "/styles.xml"] (path "modified.zip")


getNewODT :: ODT
getNewODT = do
    -- let italic = newTextStyle {textTextProps = newTextProps {fontStyle = Italic}}
    -- let underlineStyle = newTextStyle {textTextProps = newTextProps {textUnderline = Solid}}
    -- let bold = newTextStyle {textTextProps = newTextProps {fontWeight = Bold}}
    -- let boldItalic = newTextStyle {textTextProps = newTextProps {fontStyle = Italic, fontWeight = Bold, fontSize = ""}}
    -- let normal = newTextStyle 
    -- let newstyle = newTextStyle {textTextProps = newTextProps {fontStyle = Italic}, textStyleName = Just "newstyle"}
    -- let footnoteAnchor = newTextStyle {textTextProps = newTextProps {textPosition = "super 58%"}}
    let italicParaStyle = newParaStyle {paraTextProps = newTextProps {fontStyle = Italic}, paraStyleName = Just "italicPara"}

    let italicPara = ODT.p italicParaStyle "Italic para style"

    -- let italicPara = newParaStyle {paraTextProps = newTextProps {fontStyle = Italic}, paraStyleName = Just "italicPara"}
    -- let italicParaODT = toODT italicPara
    -- let newstyleodt = toODT newstyle

    let odtlst = [ 
                -- ODT.p newParaStyle ""
                --   , ODT.str "Normal parastyle text 1"
                --   , ODT.p italicPara "Italic para style"
                --   , ODT.p newParaStyle "Normal parastyle text 2"
                --   -- , ODT.p italicPara "Italic parastyle text"
                --   -- , ODT.p newParaStyle ""
                --   , ODT.span normal "Some normal text" 
                -- --   , ODT.str "This is a new string. "
                --   , ODT.span boldItalic "Some bold and italic text"
                --   , ODT.span underlineStyle "Some underlined text"
                    -- ODT.span TextStyles.bold " and some bold text." 
                --   , ODT.str " and some plain text."
                --   , ODT.span newstyle " and newstyle text"
                --   ODT.span footnoteAnchor " and footnote anchor text"
                    ODT.span TextStyles.underline " Some underlined text."
                  , italicPara
                  , ODT.span TextStyles.boldItalic " and some bold italic text."
                  , ODT.span TextStyles.normal " And some normal text"
                  , ODT.span TextStyles.underline " and some underlined text."
                  -- , ODT.p newParaStyle ""
                  ]

    let newodt = mconcat odtlst
    let newodt' = mconcat . toList $ newodt 

    newodt'

 
readWriteMonoid :: IO ()
readWriteMonoid = do

    -- unzipOdt "../doctools-data" "example2" "../doctools-data"
    -- prettifyOdt "../doctools-data" "example2"
    let italicParaStyle = newParaStyle {paraTextProps = newTextProps {fontStyle = Italic}, paraStyleName = Just "italicPara"}

    let italicPara = ODT.p italicParaStyle "Italic para style"

    -- Read files
    contentxmldoc <- XML.readFile XML.def (path $ exampleFileName <> "/content.xml")
    stylesxmldoc <- XML.readFile XML.def (path $ exampleFileName <> "/styles.xml")

    -- Write uglified files to data/
    XML.writeFile XML.def (path "content1.xml") contentxmldoc
    XML.writeFile XML.def (path "styles1.xml") stylesxmldoc

    -- Append to the word document
    let contentodtdoc = fromXMLDoc contentxmldoc
    let stylesodtdoc = fromXMLDoc stylesxmldoc

    let contentodt = getODT contentodtdoc <> getNewODT

    print $ show . paraCount $ contentodt
    print $ getText . getLastPara $ contentodt

    let contentodt' = mconcat . toList $ contentodt

    let contentodtdoc' = contentodtdoc {odt = contentodt'}

    writeODT contentodtdoc' stylesodtdoc
    

main :: IO ()
main = do
    -- readMonoid
    readWriteMonoid