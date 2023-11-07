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
import Text.ODT.Query
import qualified Text.ODT.Ops as ODT
import qualified Text.ODT.ODT as ODTType
import Text.ODT.Style

exampleFileName = "example2"


unzipFiles :: IO ()
unzipFiles = do
    -- Unzip
    Z.unzip (path $ exampleFileName <> ".odt") (path $ "/" <> exampleFileName)


    -- Produce a prettified version of the original files
    prettifyFile (path $ exampleFileName <> "/styles.xml") (path "styles2.xml")
    prettifyFile (path "content1.xml") (path "content2.xml")


writeODT :: Doc -> Doc -> IO ()
writeODT contentdoc stylesdoc = do

    let contentxmldoc = toXMLDoc contentdoc
    -- putStrLn "\n"
    -- print contentxmldoc
    let stylesxmldoc = toXMLDoc stylesdoc

    -- -- Read files
    -- contentxmldoc <- X.readFile X.def (path $ exampleFileName <> "/content.xml")
    -- stylesxmldoc <- X.readFile X.def (path $ exampleFileName <> "/styles.xml")

    -- Write modified documents back to file
    X.writeFile X.def (path "content3.xml") contentxmldoc
    X.writeFile X.def (path $ exampleFileName <> "/content.xml") contentxmldoc
    X.writeFile X.def (path $ exampleFileName <> "/styles.xml") stylesxmldoc
    X.writeFile X.def (path "styles3.xml") stylesxmldoc

    -- Produce a prettified version of the files
    prettifyFile (path "content1.xml") (path "content2.xml")
    prettifyFile (path "content3.xml") (path "content4.xml")
    prettifyFile (path "styles3.xml") (path "styles4.xml")

    -- Zip modified files
    Z.zipODT (path $ exampleFileName <> ".odt") ([path $ exampleFileName <> "/content.xml", path $ exampleFileName <> "/styles.xml"]) (path "modified.odt")
    Z.zipODT (path $ exampleFileName <> ".odt") ([path $ exampleFileName <> "/content.xml", path $ exampleFileName <> "/styles.xml"]) (path "modified.zip")


getNewODT :: ODT
getNewODT = do
    let italic = newTextStyle {textTextProps = newTextProps {fontStyle = Italic}}
    let underlineStyle = newTextStyle {textTextProps = newTextProps {underline = Solid}}
    let bold = newTextStyle {textTextProps = newTextProps {fontWeight = Bold}}
    let boldItalic = newTextStyle {textTextProps = newTextProps {fontStyle = Italic, fontWeight = Bold, fontSize = ""}}
    let normal = newTextStyle 
    let newstyle = newTextStyle {textTextProps = newTextProps {fontStyle = Italic}, textStyleName = Just "newstyle"}
    let footnoteAnchor = newTextStyle {textTextProps = newTextProps {textPosition = "super 58%"}}

    let italicPara = newParaStyle {paraTextProps = newTextProps {fontStyle = Italic}, paraStyleName = Just "italicPara"}
    let italicParaODT = toODT italicPara
    -- putStrLn $ "\nitalicParaODT: " <> show italicParaODT <> "\n"

    let newstyleodt = toODT newstyle

    let odtlst = [ ODT.p newParaStyle ""
                  , ODT.str "Normal parastyle text 1"
                  , ODT.p italicPara "Italic para style"
                  , ODT.p newParaStyle "Normal parastyle text 2"
                  -- , ODT.p italicPara "Italic parastyle text"
                  -- , ODT.p newParaStyle ""
                  , ODT.span normal "Some normal text" 
                --   , ODT.str "This is a new string. "
                  , ODT.span boldItalic "Some bold and italic text"
                  , ODT.span underlineStyle "Some underlined text"
                  , ODT.span bold " and some bold text." 
                  , ODT.str " and some plain text."
                  , ODT.span newstyle " and newstyle text"
                  , ODT.span footnoteAnchor " and footnote anchor text"
                  -- , ODT.p newParaStyle ""
                  ]

    -- let odtlst = [ODT.p italicPara "Italic para style"]
    let newodt = mconcat odtlst
    let newodt' = mconcat . toList $ newodt 

    newodt'


 
readWriteMonoid :: IO ()
readWriteMonoid = do

    unzipFiles

    -- Read files
    contentxmldoc <- X.readFile X.def (path $ exampleFileName <> "/content.xml")
    stylesxmldoc <- X.readFile X.def (path $ exampleFileName <> "/styles.xml")

    -- Write uglified files to data/
    X.writeFile X.def (path "content1.xml") contentxmldoc
    X.writeFile X.def (path "styles1.xml") stylesxmldoc

    -- Append to the word document
    let contentodtdoc = fromXMLDoc contentxmldoc
    let stylesodtdoc = fromXMLDoc stylesxmldoc

    let contentodt = getNewODT <> getODT contentodtdoc

    print $ show . length . getParas $ contentodt
    -- print contentodt 

    let contentodt' = mconcat . toList $ contentodt

    let contentodtdoc' = contentodtdoc {odt = contentodt'}

    writeODT contentodtdoc' stylesodtdoc
    


main :: IO ()
main = do
    -- readMonoid
    readWriteMonoid