module Fraction (Fraction
                , toFloat
                , truncate'
                , add
                , sub
                , mul
                , div'
                , mod'
                , compare'
                ) where

-- |An integer pair (a,b) representing the fraction a/b. 0 is (0, 0)
type Fraction = (Integer, Integer)

-- |Truncate a fraction
truncate' :: Fraction -> Int
truncate' (0, 0) = 0
truncate' (a, b) = fromInteger $ a `div` b

-- |Convert a Fraction into a Float
toFloat :: Fraction -> Float
toFloat (0, 0) = 0
toFloat (a, b) = fromInteger a / fromInteger b

-- |Add two Fractions
add :: Fraction -> Fraction -> Fraction
add (a,b) (c,d) = if a == 0 || c == 0
                      then (a + c, b + d)
                      else let f = a * d + c * b
                               g = if f == 0 then 0 else b * d
                               h = if f == 0 then 1 else gcd f g
                           in (f `div` h, g `div` h)
-- |Subtract two fractions
sub :: Fraction -> Fraction -> Fraction
sub (a,b) (c,d) = if a == 0 || c == 0
                      then (a - c, b + d)
                      else let f = a * d - c * b
                               g = if f == 0 then 0 else b * d
                               h = if f == 0 then 1 else gcd f g
                           in (f `div` h, g `div` h)

-- |Multiply two fractions
mul :: Fraction -> Fraction -> Fraction
mul (a,b) (c,d) = let f = a * c
                      g = b * d
                      h = if f == 0 then 1 else gcd f g
                  in (f `div` h, g `div` h)

-- |Divide two fractions
div' :: Fraction -> Fraction -> Fraction
div' (a,b) (c,d) = if c == 0
                       then error "Something smells fishy..."
                       else let f = a * d
                                g = b * c
                                h = if f == 0 then 1 else gcd f g
                            in (f `div` h, g `div` h)

-- |Modulatify two fractions
mod' :: Fraction -> Fraction -> Fraction
mod' (a,b) (c,d) = if b == 1 && d == 1 && a `mod` c /= 0
                       then (a `mod` c, 1)
                       else (0        , 0)

-- |Compare two fractions
compare' :: Fraction -> Fraction -> Ordering
compare' (a,b) (c,d) = let f = a * lcm b d
                           g = d * lcm b d
                       in if f == g
                              then EQ
                              else if f > g
                                       then GT
                                       else LT
