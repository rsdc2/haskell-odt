module Text.ODT.File (path) where

type Filepath = String 

path :: String -> Filepath
-- path s = "/data/programming/haskell/doctools-data/" <> s
path s = "../doctools-data/" <> s
