module Acronym (abbreviate) where
import Data.Char

abbreviate :: String -> String
abbreviate xs = concatMap (\x -> let x' = filter isAlpha x
                                     h = toUpper (head x')
                                     t = tail x in
                                     if all isUpper t
                                     then [h]
                                     else h : filter isUpper t) (words (repl xs))
                                     where repl [] = []
                                           repl ('-':ys) = ' ' : repl ys
                                           repl (y:ys) = y : repl ys
