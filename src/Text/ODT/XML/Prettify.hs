module Text.ODT.XML.Prettify ( prettifyFile ) where

import qualified Text.XML.Light as XmlLight

type Filepath = String

-- Take an input filepath and an output filepath for an XML file
-- Write out a prettified XML file to the output path
prettifyFile :: Filepath -> Filepath -> IO ()
prettifyFile fpIn fpOut = do
    xmlString <- readFile fpIn
    let docElement = XmlLight.parseXMLDoc xmlString
    let xmlStringPretty = XmlLight.ppElement <$> docElement
    case xmlStringPretty of
        Nothing -> return ()
        Just x -> writeFile fpOut x