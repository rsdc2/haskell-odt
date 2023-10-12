module Text.ODT.XML.Prettify
    ( prettifyFile
    ) where

import qualified Text.XML.Light as XL

type Filepath = String

prettifyFile :: Filepath -> Filepath -> IO ()
prettifyFile fpIn fpOut = do
    s <- readFile fpIn
    let element = XL.parseXMLDoc s
    let maybeS = XL.ppElement <$> element
    case maybeS of
        Nothing -> return ()
        Just x -> writeFile fpOut x