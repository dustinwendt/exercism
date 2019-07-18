module Clock (addDelta, fromHourMin, toString) where

data Clock = C Int Int

instance Eq Clock where
  (C h m) == (C h' m') = h == h' && m == m'

fromHourMin :: Int -> Int -> Clock
fromHourMin a b = let c = b `div` 60 in 
                    C ((a + c) `mod` 24) (b `mod` 60)

toString :: Clock -> String
toString (C h m) = f h ++ ":" ++ f m
                   where f g | g > 9     = show g
                             | otherwise = '0' : show g

addDelta :: Int -> Int -> Clock -> Clock
addDelta 0 0  c = c
addDelta h m (C h' m') = let z = (m + m') `div` 60 in
                             C ((h' + h + z) `mod` 24) ((m' + m) `mod` 60)
