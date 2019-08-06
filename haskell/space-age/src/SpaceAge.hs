module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / earthYear planet
                   where earthYear p = case p of
                                    Mercury -> e * 0.2408476
                                    Venus   -> e * 0.61519726
                                    Earth   -> e
                                    Mars    -> e * 1.8808158
                                    Jupiter -> e * 11.862615
                                    Saturn  -> e * 29.447498
                                    Uranus  -> e * 84.016846
                                    Neptune -> e * 164.79132
                                    where e = 31557600
