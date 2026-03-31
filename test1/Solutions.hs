module Solutions where

import Data.List (sort)

-- leftMin
leftMin :: [Int] -> [Int]
leftMin [] = []
leftMin (x : xs) = go x xs
 where
  go :: Int -> [Int] -> [Int]
  go minEl [] = [minEl]
  go minEl (x : xs) =
    minEl : go (min x minEl) xs

-- >>> minimum [3,5,2,7,1]
-- 1

-- >>> leftMin [3,5,2,7,1]
-- [3,3,2,2,1]

-- >>> leftMin [5,4,3,2,1]
-- [5,4,3,2,1]

-- [0,1,3,6]
-- (((0 + 1) + 2) + 3) ~> 6

-- >>> foldl (+) 0 [1,2,3]
-- 6

-- [maxBound,3,3,2,2,1] -> tail
-- (min (min (min (min maxBound 3) 5) 7) ... 1)

-- >>> foldl min (maxBound :: Int) [3,5,2,7,1]
-- 1

-- >>> :t +d foldl
-- foldl :: (b -> a -> b) -> b -> [a] -> b

-- >>> :t scanl
-- scanl :: (b -> a -> b) -> b -> [a] -> [b]

-- >>> scanl (+) 0 [1,2,3]
-- [0,1,3,6]

-- >>> tail $ scanl min (maxBound :: Int) [3,5,2,7,1]
-- [3,3,2,2,1]

-- INT_MAX

-- min n maxBound ~> n

-- >>> max (-100000000) (minBound :: Int)
-- -100000000

leftMin' :: [Int] -> [Int]
leftMin' lst = tail $ scanl min maxBound lst

-- >>> maxBound :: Char
-- '\1114111'

-- >>> leftMin' [5,4..1]
-- [5,4,3,2,1]

-- rightSum

-- [1,2,3] ~> [6,5,3]
-- 1 + (2 + (3 + 0))

-- >>> :t +d foldr
-- foldr :: (a -> b -> b) -> b -> [a] -> b

-- >>> :t scanr
-- scanr :: (a -> b -> b) -> b -> [a] -> [b]

-- >>> scanr (+) 0 [1,2,3]
-- [6,5,3,0] ~> init

rightSum :: [Int] -> [Int]
rightSum lst = init $ scanr (+) 0 lst

-- >>> rightSum [1,1,1]
-- [3,2,1]

kthTransformed :: [Int] -> (Int -> Int) -> (Int -> Bool) -> Int -> Int
kthTransformed lst f p k = res2 !! (k - 1)
 where
  res1 = filter p $ map f lst
  res2 = reverse $ sort res1

-- >>> [1,2,3] !! 3

-- *** Exception: Prelude.!!: index too large

-- 16
-- [1,4,9,16,25]
-- [4,16]
-- [16,4]
-- >>> kthTransformed [1,2,3,4,5] (\x -> x*x) even 1
-- 16

kthOriginal :: [Int] -> (Int -> Int) -> (Int -> Bool) -> Int -> Int
kthOriginal lst f p k
  | k <= 0 || k > length res2 = error "no such number"
  | otherwise = res2 !! (k - 1)
 where
  -- [1,2,3,4,5]
  -- [1,4,9,16,25]
  -- xs = [(1,1),(2,4),(3,9),(4,16),(5,25)] :: [(Int,Int)]
  -- filter (\(_,y) -> p y)

  -- xs' = [(2,4),(4,16)] :: [(Int,Int)]
  -- xs''= [2,4]

  -- [4,16]
  -- k = 1 !~> 4
  -- k = 1  ~> 2

  res1 = map fst $ filter (\(_, y) -> p y) $ lst `zip` map f lst
  res2 = sort res1

-- >>> kthOriginal [1,2,3,4,5] (\x -> x*x) even 2
-- 4
