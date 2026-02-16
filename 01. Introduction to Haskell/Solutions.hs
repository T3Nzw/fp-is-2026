-- int add(int x, int y) {...}

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

