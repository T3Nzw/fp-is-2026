-- int add(int x, int y) {...}

import Prelude hiding (Rational (..))

add :: Int -> Int -> Int
add x y = x + y

add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z

foo :: Int
foo = 42

sphereVolume :: Double -> Double
sphereVolume r = 4 / 3 * pi * r ^ 3

hasRealRoots :: Double -> Double -> Double -> Bool
hasRealRoots a b c = discriminant >= 0
 where
  discriminant = b ^ 2 - 4 * a * c

-- distance :: (Double, Double) -> (Double, Double) -> Double
-- distance p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

-- distance :: (Double, Double) -> (Double, Double) -> Double
-- distance p1 p2 = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
--   where
--    x1 = fst p1
--    x2 = fst p2
--    y1 = snd p1
--    y2 = snd p2

distance :: (Double, Double) -> (Double, Double) -> Double
distance (x1, y1) (x2, y2) =
  sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

{-
if (a > b) {
  return a;
}
else {
  return b;
}

return a > b ? a : b;
-}

max' :: Int -> Int -> Int
max' a b = if a > b then a else b

max3 :: Int -> Int -> Int -> Int
max3 a b c = max' (max' a b) c

-- if (n == 0) return true;
-- return false;

-- return n == 0;

isZero :: Int -> Bool
isZero n = n == 0

{-
switch (n) {
  case 0: return true;
  default: return false;
}
-}

isZero' :: Int -> Bool
isZero' 0 = True
isZero' _ = False

(&&&) :: Bool -> Bool -> Bool
True &&& True = True
_ &&& _ = False

type Point = (Double, Double)

triangleArea :: Point -> Point -> Point -> Double
triangleArea p1 p2 p3 = sqrt (semip * (semip - side1) * (semip - side2) * (semip - side3))
 where
  side1 = distance p1 p2
  side2 = distance p1 p3
  side3 = distance p2 p3
  semip = (side1 + side2 + side3) / 2

hms :: Int -> (Int, Int, Int)
hms seconds = (hours, minutes, rem2)
 where
  (hours, rem1) = seconds `divMod` 3600
  (minutes, rem2) = rem1 `divMod` 60

-- typedef, using

type Rational = (Int, Int)

mkRat :: Int -> Int -> Rational
mkRat numerator denominator =
  if denominator == 0
    then error "denominator cannot be zero"
    else simplify (numerator, denominator)

{-
int foo(int x) {
  if (x == 2) {
    return 42;
  }
}
-}

mkRat' :: Int -> Int -> Rational
mkRat' numerator denominator
  | denominator == 0 = error "denominator cannot be zero"
  | otherwise = simplify (numerator, denominator)

mkRat'' :: Int -> Int -> Rational
mkRat'' _ 0 = error "denominator cannot be zero"
mkRat'' numerator denominator = simplify (numerator, denominator)

-- тогава в горните функции ще искаме да опростяваме директно
-- при създаването на рационално число
simplify :: Rational -> Rational
simplify (numer, denom) = (numer `div` ratGcd, denom `div` ratGcd)
 where
  ratGcd = gcd numer denom

addRat :: Rational -> Rational -> Rational
addRat (n1, d1) (n2, d2) = simplify (numer, denom)
 where
  numer = n1 * d2 + n2 * d1
  denom = d1 * d2
