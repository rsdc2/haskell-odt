{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Text.ODT.XML.Doc (
      docElem
    , emptyMisc
    , emptyPrologue
    , Epilogue
    , docName
    , prologue
    , epilogue
    , getNamespaces
    , hasName
    , HasNodeChildren(..)
    , Namespaces
)

where  

import Text.XML ( 
      Document(..)
    , Element(..)
    , Miscellaneous
    , Prologue (..)
    , Name(..) 
    , Node(..)
    , def )


import qualified Data.Text as T
import qualified Data.Map as Map

import Text.ODT.XML.Types

type Epilogue = [Miscellaneous]
type Namespaces = Map.Map Name T.Text

instance HasNodeChildren Document where
  getChildren :: Document -> [Node]
  getChildren (Document _ (Element _ _ children) _) = children

  appendChildren :: [Node] -> Document -> Document
  appendChildren ns (Document prlg (Element name attrs children) eplg) = Document prlg (Element name attrs (children <> ns)) eplg

docElem :: Document -> Element
docElem (Document _ elem _) = elem

docName :: Document -> T.Text
docName (Document _ (Element (Name n _ _) _ _) _) = n

emptyMisc :: [Miscellaneous]
emptyMisc = []

emptyPrologue :: Prologue
emptyPrologue = Prologue [] Nothing []

epilogue :: Document -> Epilogue
epilogue (Document prlg elem eplg) = eplg

getNamespaces :: Document -> Namespaces
getNamespaces (Document _ (Element _ namespaces _) _) = namespaces

prologue :: Document -> Prologue
prologue (Document prlg elem eplg) = prlg