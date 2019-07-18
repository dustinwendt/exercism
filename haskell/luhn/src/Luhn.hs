module Luhn (isValid) where
import Data.Char

-- 1) Double every second digit starting from the right
--    If doubling is greater than 9, then subtract 9
-- 2) sum all the digits
-- 3) If sum % 10 == 0 then True else False

isValid :: String -> Bool
isValid n = let n' = transform $ map (read . pure :: Char -> Int) . reverse $ filter (not . isSpace) n in
  if length n' > 1
  then sum n' `mod` 10 == 0
  else False

transform :: [Int] -> [Int]
transform []    = []
transform [x] = [x]
transform (x:y:xs) = x : f y : transform xs
  where f a = let d = 2 * a in
          if d > 9 then d - 9 else d




