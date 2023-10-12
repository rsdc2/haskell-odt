module Text.ODT.Utils.List (
      headM
    , maybeToList) 

where

headM :: [a] -> Maybe a
headM [] = Nothing
headM as = Just (head as)

maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = []

