module Text.ODT.XML.Prettify
    ( prettifyFile
    ) where

import qualified Text.XML.Light as XmlLight

type Filepath = String

-- Take an input filepath and an output filepath for an XML file
-- Write out a prettified XML file to the output path
prettifyFile :: Filepath -> Filepath -> IO ()
prettifyFile fpIn fpOut = do
    s <- readFile fpIn
    let element = XmlLight.parseXMLDoc s
    let maybeS = XmlLight.ppElement <$> element
    case maybeS of
        Nothing -> return ()
        Just x -> writeFile fpOut x