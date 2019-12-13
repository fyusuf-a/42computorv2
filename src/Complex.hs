module Complex where

import Data.Ratio 

data Complex = Complex
                  { real :: Rational
                  , imaginary :: Rational
                  }

class PrettyShow a where
  pretty :: a -> String

instance PrettyShow Integer where
  pretty x = show x

instance (Eq a, Num a, PrettyShow a) => PrettyShow (Ratio a) where
  pretty r
    | d == fromInteger 1 = pretty n
    | otherwise = pretty n ++ "/" ++ pretty d
      where d = denominator r
            n = numerator r

instance PrettyShow Complex where
  pretty (Complex 0 0) = "0"
  pretty (Complex x 0) = pretty x
  pretty (Complex 0 1) = "i"
  pretty (Complex 0 y) = pretty y ++ "i"
  pretty (Complex x y) = pretty x ++ " + " ++ pretty y ++ "i"

instance Num Complex where
  (Complex x y) + (Complex x' y') = Complex (x + x') (y + y') 
  (Complex x y) - (Complex x' y') = Complex (x - x') (y - y') 
  (Complex x y) * (Complex x' y') = Complex (x * x' - y * y') (x * y' + x' * y) 
  negate (Complex x y) = Complex (-x) (-y)
  -- Modulus
  abs (Complex x 0) = Complex (abs x) 0
  abs _ = error "Cannot compute absolute value if complex is not a real"
  signum (Complex x 0) = Complex (signum x) 0
  signum _ = error "Cannot compute signum if complex is not a real"
  fromInteger x = Complex (fromInteger x) 0

conjugate :: Complex -> Complex
conjugate (Complex x y) = Complex x (-y)

instance Fractional Complex where
  (Complex _ _) / (Complex 0 0) = error "Division by zero is impossible"
  (Complex x y) / (Complex x' y') = Complex ((x * x' + y * y')/mod) ((-x * y' + y * x')/mod)
    where mod = x'^2 + y'^2
  fromRational r = Complex (fromRational r) 0
