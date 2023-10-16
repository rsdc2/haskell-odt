{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Text.XML as X

import Text.ODT.File
import Text.ODT.XML.Prettify
import qualified Text.ODT.Zip.Zip as Z 
import Text.ODT.Utils.Types (
      IsText(..)
    , Stringable(..))
import Text.ODT.ODT
import Text.ODT.Doc
import Text.ODT.Archive
import qualified Text.ODT.Ops as ODT
import qualified Text.ODT.ODT as ODTType
import Text.ODT.Style

exampleFileName = "example2"

 
readWriteMonoid :: IO ()
readWriteMonoid = do
    -- Unzip
    Z.unzip (path $ exampleFileName <> ".odt") (path $ "/" <> exampleFileName)

    -- Read files
    contentxmldoc <- X.readFile X.def (path $ exampleFileName <> "/content.xml")
    stylesxmldoc <- X.readFile X.def (path $ exampleFileName <> "/styles.xml")

    -- Write uglified files to data/
    X.writeFile X.def (path "content1.xml") contentxmldoc
    X.writeFile X.def (path "styles1.xml") stylesxmldoc

    -- Produce a prettified version of the original files
    prettifyFile (path $ exampleFileName <> "/styles.xml") (path "styles2.xml")
    prettifyFile (path "content1.xml") (path "content2.xml")

    -- Append to the word document
    let contentodtdoc = fromXMLDoc contentxmldoc
    let stylesodtdoc = fromXMLDoc stylesxmldoc

    print stylesodtdoc 
    putStrLn "\n"

    let contentodt = odtFromODTDoc contentodtdoc
    print contentodt

    let italic = newTextStyle {textTextProps = newTextProps {fontStyle = Italic}}
    let underlineStyle = newTextStyle {textTextProps = newTextProps {underline = Solid}}
    let bold = newTextStyle {textTextProps = newTextProps {fontWeight = Bold}}
    let boldItalic = newTextStyle {textTextProps = newTextProps {fontStyle = Italic, fontWeight = Bold, fontSize = ""}}
    let normal = newTextStyle 
    let newstyle = newTextStyle {textTextProps = newTextProps {fontStyle = Italic}, textStyleName = Just "newstyle"}
    let footnoteAnchor = newTextStyle {textTextProps = newTextProps {textPosition = "super 58%"}}

    let italicPara = newParaStyle {paraTextProps = newTextProps {fontStyle = Italic}, paraStyleName = Just "italicPara"}
    let italicParaODT = toODT italicPara
    putStrLn $ "\nitalicParaODT: " <> show italicParaODT <> "\n"

    let newstyleodt = toODT newstyle
    let italicParaODT = toODT italicPara

    let archive = Archive {
        -- contentDoc = appendODT italicParaODT contentodtdoc
        contentDoc = contentodtdoc
      , stylesDoc = appendODT newstyleodt . appendODT italicParaODT $ stylesodtdoc  -- . appendODT italicParaODT $ 
    } 
    -- let archive = Archive {contentDoc = contentodtdoc, stylesDoc = appendODT newstyleodt stylesodtdoc} 
    -- let archive = Archive {contentDoc = contentodtdoc, stylesDoc = stylesodtdoc} 
    -- let newodt = ODT.span bold " some bold text"
    let newodt = mconcat [ ODT.p newParaStyle "Normal parastyle text 1"
                          , ODT.p italicPara "Italic para style"
                          , ODT.p newParaStyle "Normal parastyle text 2"
                          -- , ODT.p italicPara "Italic parastyle text"
                          -- , ODT.p newParaStyle ""
                          , ODT.span normal "Some normal text" 
                        --   , ODT.str "This is a new string. "
                          , ODT.span boldItalic "Some bold and italic text"
                          , ODT.span underlineStyle "Some underlined text"
                          , ODT.span bold " and some bold text." 
                          , ODT.span newstyle " and newstyle text"
                          , ODT.span footnoteAnchor " and footnote anchor text"
                          -- , ODT.p newParaStyle ""
                          ]

    -- let newodt = ODT.p <> ODT.span bold "bold text" <> ODT.span italic "italic text" <> ODT.span newstyle "newstyle text"
    

    -- let newodt = ODT.p <> ODT.span boldItalic "Some bold and italic text" <> ODT.span bold " and some bold text" <> ODT.span footnoteAnchor " and footnote anchor text"
    -- let newodt = fromList [ODT.p, ODT.span boldItalic "Some bold and italic text", ODT.span bold " and some bold text"]
    -- let newodt = mconcat [ODT.p, ODT.span boldItalic "Some bold and italic text", ODT.span bold " and some bold text"]

    -- putStrLn "newodt: \n"
    -- putStrLn $ show $ getLastODT newodt
    -- putStrLn $ show $ removeLastODT newodt
    -- putStrLn "\n"

    -- let contentxmldoc' = toXMLDoc . contentDoc $ appendODT (fromList odtlist) archive
    let contentxmldoc' = toXMLDoc . contentDoc $ appendODT newodt  archive

    let stylesxmldoc' = toXMLDoc . stylesDoc $ archive

    -- Write modified documents back to file
    X.writeFile X.def (path "content3.xml") contentxmldoc'
    X.writeFile X.def (path $ exampleFileName <> "/content.xml") contentxmldoc'
    X.writeFile X.def (path $ exampleFileName <> "/styles.xml") stylesxmldoc'
    X.writeFile X.def (path "styles3.xml") stylesxmldoc'

    -- Produce a prettified version of the files
    prettifyFile (path "content3.xml") (path "content4.xml")
    prettifyFile (path "styles3.xml") (path "styles4.xml")

    -- Zip modified files
    Z.zipODT (path $ exampleFileName <> ".odt") ([path $ exampleFileName <> "/content.xml", path $ exampleFileName <> "/styles.xml"]) (path "modified.odt")
    Z.zipODT (path $ exampleFileName <> ".odt") ([path $ exampleFileName <> "/content.xml", path $ exampleFileName <> "/styles.xml"]) (path "modified.zip")
    -- Z.zip (path "example2") (path "modified.zip")

main :: IO ()
main = do
    -- readMonoid
    readWriteMonoid