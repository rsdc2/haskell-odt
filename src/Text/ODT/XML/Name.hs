{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Text.ODT.XML.Name (
    HasXMLName(..)
)

where


import Text.XML ( Element(..), Node(..), Name(..) )
import Data.Text as T

import Text.ODT.XML.Types ( HasXMLName(..) )
import Text.ODT.XML.Doc ( Namespaces )

instance HasXMLName Name where 
    getLabel :: Name -> T.Text
    getLabel (Name _ _ (Just getLabel)) = getLabel
    getLabel (Name _ _ Nothing) = ""

    getLocalName :: Name -> T.Text
    getLocalName (Name n _ _) = n

    hasLocalName :: T.Text -> Name -> Bool
    hasLocalName n1 (Name n2 _ _) = n1 == n2 

    hasName :: T.Text -> T.Text -> Name -> Bool
    hasName name1 namespace1 (Name name2 namespace2 _) = name1 == name2 && (Just namespace1) == namespace2