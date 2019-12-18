module Complex where

import Data.Ratio 
import Data.Maybe (isJust, fromJust) 
import Control.Exception (ArithException(..), NoMethodError(..), throw)

data Complex = Complex
                  { real :: Rational
                  , imaginary :: Rational
                  }
    deriving Show

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
  pretty (Complex 0 (-1)) = "-i"
  pretty (Complex 0 y) = pretty y ++ "i"
  pretty (Complex x y)
    | y == 1 = pretty x ++ " + i"
    | y == -1 = pretty x ++ " - i"
    | y > 0 = pretty x ++ " + " ++ pretty y ++ "i"
    | otherwise = pretty x ++ " - " ++ pretty (-y) ++ "i"

instance Num Complex where
  (Complex x y) + (Complex x' y') = Complex (x + x') (y + y') 
  (Complex x y) - (Complex x' y') = Complex (x - x') (y - y') 
  (Complex x y) * (Complex x' y') = Complex (x * x' - y * y') (x * y' + x' * y) 
  negate (Complex x y) = Complex (-x) (-y)
  -- Modulus
  abs (Complex x 0) = Complex (abs x) 0
  abs _ = throw $ NoMethodError "cannot compute absolute value if complex is not a real"
  signum (Complex x 0) = Complex (signum x) 0
  signum _ = throw $ NoMethodError "cannot compute signum if complex is not a real"
  fromInteger x = Complex (fromInteger x) 0

conjugate :: Complex -> Complex
conjugate (Complex x y) = Complex x (-y)

i :: Complex
i = Complex 0 1

(^^^) :: Complex -> Complex -> Complex
a ^^^ b
  | isJust maybeN =  a ^^ (fromJust maybeN)
  | otherwise = throw $ NoMethodError $ "power " ++ pretty b ++ " is not a natural"
  where toNatural (Complex x y)
          | denominator x == 1 && y == 0 = Just $ numerator x
          | otherwise = Nothing
        maybeN = toNatural b

instance Fractional Complex where
  (Complex _ _) / (Complex 0 0) = throw DivideByZero
  (Complex x y) / (Complex x' y') = Complex ((x * x' + y * y')/mod) ((-x * y' + y * x')/mod)
    where mod = x'^2 + y'^2
  fromRational r = Complex (fromRational r) 0
