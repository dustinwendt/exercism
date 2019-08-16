module Pangram (isPangram) where

import Data.Char

isPangram :: String -> Bool
isPangram text = null $ words (stripChars (map toLower text) ['a'..'z'])
                    where stripChars = filter . flip notElem
