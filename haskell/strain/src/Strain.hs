module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard _ [] = []
discard p (x:xs) = let rest = discard p xs in
                   if p x
                   then rest
                   else x : rest

keep :: (a -> Bool) -> [a] -> [a]
keep _ []     = []
keep p (x:xs) = let rest = keep p xs in
                    if p x
                    then x : rest 
                    else rest
