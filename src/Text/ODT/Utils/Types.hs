{-# LANGUAGE OverloadedStrings #-}

module Text.ODT.Utils.Types 
    ( IsText(..)
    , Stringable(..)
    ) where

import qualified Data.Text as T

class IsText a where
    toText :: a -> T.Text

class Stringable a where
    toString :: a -> T.Text

