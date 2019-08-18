module School (School, add, empty, grade, sorted) where

import Data.List

type School = [(Int, [String ])]

add :: Int -> String -> School -> School
add gradeNum student [] = [(gradeNum, [student])]
add gradeNum student ((g,s):cs)
  | g == gradeNum = (g, s ++ [student]):cs
  | otherwise = (g,s): (add gradeNum student cs)

empty :: School
empty = []

grade :: Int -> School -> [String]
grade _ [] = []
grade gradeNum ((g,s):cs) | g == gradeNum = s
                          | otherwise     = grade gradeNum cs

sorted :: School -> [(Int, [String])]
sorted school = sort (map (g sort) school)
                 where g f (a,b) = (a, f b)
