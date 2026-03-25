module Solutions where

import Prelude hiding (all, filter, foldl, foldl1, foldr, foldr1, lookup, map, reverse, zipWith, (.))

add :: Int -> (Int -> Int)
add x y = x + y

-- add 1
-- add y = 1 + y

plusOne :: Int -> Int
plusOne x = x + 1

mapInt :: (Int -> Int) -> (Int -> Int)
-- f :: Int -> Int
-- x :: Int
-- f x :: Int
mapInt f x = f x

-- >>> mapInt (+1) 6

-- mapInt :: Int -> (Int -> (Int -> Int))

-- f :: forall a b. a -> b -> b
-- f _ y = y

map :: (a -> b) -> [a] -> [b]
-- [] :: [a]
-- [] :: [b]
-- [] наричаме полиморфна константа
map _ [] = []
-- (Int -> Int) -> [Int] -> [Int]
-- a ~ Int
-- b ~ Int
-- (Int -> Char) -> [Int] -> [Char]
-- x :: Int
-- map f xs :: [Char]
-- x : map f xs

-- в общия случай а е различно от b
map f (x : xs) = f x : map f xs

-- f :: a -> b

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs) =
  if p x
    then x : filter p xs
    else filter p xs

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all p (x : xs) = p x && all p xs

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
-- op x y
-- x `op` y
zipWith op (x : xs) (y : ys) = x `op` y : zipWith op xs ys

-- zip :: [a] -> [b] -> [(a,b)]

zipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zip xs ys :: [(a,b)]
-- map ???
-- (x,y) -> x `op` y
-- (x,y) :: (a,b)
-- op :: a -> b -> c
-- op' :: (a,b) -> c
-- map (\(x,y) -> x `op` y) (zip xs ys)
zipWith2 op xs ys = map (uncurry op) (zip xs ys)

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 op (x, y) = x `op` y

-- splitAt
span2 :: (a -> Bool) -> [a] -> ([a], [a])
span2 p lst = (takeWhile p lst, dropWhile p lst)

-- filter :: (a -> Bool) -> [a] -> [a]
fixpoints :: (Int -> Int) -> Int -> Int -> [Int]
fixpoints f a b = filter (\x -> f x == x) [a .. b]

fixpointsCnt :: (Int -> Int) -> Int -> Int -> Int
fixpointsCnt f a b = length $ fixpoints f a b

compose :: (b -> c) -> (a -> b) -> (a -> c)
-- f :: a -> b
-- g :: b -> c
-- x :: a
-- f x :: b
-- g (f x) :: c
compose g f = \x -> g (f x)

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) = compose

-- a = Int
-- (Int -> Int -> Int) -> Int -> [Int] -> Int
foldr :: (a -> b -> b) -> b -> [a] -> b
-- 0
foldr _ nv [] = nv
-- [1,2,3]
-- op = (+)
-- 1 + sum([2,3])
-- ...
-- 1 + (2 + (3 + 0))
foldr op nv (x : xs) = x `op` foldr op nv xs

-- [1,2,3]
-- 1 : (2 : (3 : []))
-- op = (:)
-- nv = []
-- lst :: [Int]

-- f 1 : (f 2 : (f 3 : []))
map' :: (a -> b) -> [a] -> [b]
-- x е елемент на списъка
-- xs е резултатът от рекурсивното извикване
map' f lst = foldr (\x xs -> f x : xs) [] lst

filter' :: (a -> Bool) -> [a] -> [a]
filter' p lst = foldr (\x xs -> if p x then x : xs else xs) [] lst

-- [1,2,3]
-- (((0 + 1) + 2) + 3)
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ nv [] = nv
foldl op nv (x : xs) = foldl op (nv `op` x) xs

-- първоначално λ-функцията ни беше:
-- \xs x -> f x : xs
-- но видяхме, че тя обръща списъка, защото:
-- nv = []
-- 1. x = 1, xs = []
--    f x : xs ~> 2 : [] = [2]
-- 2. x = 2, xs = [2]
--    f x : xs ~> 3 : [2] = [3,2]
-- 3. x = 3, xs = [3,2]
--    f x : xs ~> 4 : [3,2] = [4,3,2]
map'' :: (a -> b) -> [a] -> [b]
map'' f lst = foldl (\xs x -> xs ++ [f x]) [] lst

reverse :: [a] -> [a]
reverse lst = foldl (\xs x -> x : xs) [] lst

lookup :: (a -> Bool) -> [(a, b)] -> b
lookup _ [] = error "could not find key"
lookup p ((key, value) : xs)
  | p key = value
  | otherwise = lookup p xs

findIndex :: (a -> Bool) -> [a] -> (Int, a)
-- findIndex p lst = head $ filter (\(index, el) -> p el) $ zip [0 ..] lst
findIndex p lst = lookup p $ zip lst (zip [0 ..] lst)

sort :: [Int] -> [Int]
sort [] = []
sort (pivot : xs) = sort left ++ [pivot] ++ sort right
 where
  left = filter (\x -> x < pivot) xs
  -- x >= pivot === not (pivot < x)
  right = filter (\x -> not (x < pivot)) xs

sortBy :: (a -> a -> Bool) -> [a] -> [a]
sortBy _ [] = []
sortBy cmp (pivot : xs) = sortBy cmp left ++ [pivot] ++ sortBy cmp right
 where
  -- p = (<)
  -- x < pivot
  left = filter (\x -> x `cmp` pivot) xs
  right = filter (\x -> not (x `cmp` pivot)) xs

minimumBy :: (a -> a -> Bool) -> [a] -> a
-- minimumBy p lst = head $ sortBy p lst
-- cmp = < => y < ys
-- nv = x
-- y < nv => y < x
minimumBy cmp (x : xs) = foldr (\y ys -> if y `cmp` ys then y else ys) x xs

-- f = (* (-1))
-- op = (<)
-- on (<) (* (-1)) :: a -> a -> c
--                    Int -> Int -> Bool
-- примерът с горните функции е малко по-надолу :)
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- f :: a -> b
-- x :: a
-- y :: a
-- f x :: b
-- f y :: b
-- op :: b -> b -> c
-- ??? op (f x) (f x) - не искаме това, искаме да използваме всички стойности (долното)
-- op (f x) (f y) :: c
on op f x y = f x `op` f y

-- изводът от горната функция е, че можем да дефинираме функции
-- само на база типовете, които имаме, без да се интересуваме от семантиката им.

-- add3 :: Int -> (Int -> (Int -> Int))
-- add3 1 :: Int -> (Int -> Int)
-- add3 1 2 :: Int -> Int
add3 :: Int -> (Int -> (Int -> Int))
add3 = undefined

-- примерът, който бяхме показали с minimumBy и on:
-- искаме да дефинираме функция, която приема списък от числа l
-- и едноместна функция f, която трансформира елементите по някакъв начин,
-- след което да намерим минималния елемент minEl в трансформирания списък tl
-- и да върнем оригиналния елемент от l (т.е. първообразът на minEl под f).
-- например:

-- за списъка [3,2,1] и функцията (* (-1)) (т.е. \x -> x * (-1))
-- получаваме списъка [-3,-2,-1], откъдето минималният елемент е -3,
-- и сам по себе си -3 е бил получен от 3, така че връщаме 3 (първообраза)

-- >>> minimumBy (on (<) (* (-1))) [3,2,1]
-- 3

-- >>> minimumBy ((<) `on` (* (-1))) [3,2,1]
-- 3

toDigits :: Int -> [Int]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

fromDigits :: [Int] -> Int
-- [1,2,3] -> 123
-- (((0*10 + 1)*10 + 2)*10 + 3)
fromDigits lst = foldl (\acc x -> acc * 10 + x) 0 lst

sortDigits :: Int -> Int
sortDigits n = fromDigits $ sortedEvens ++ sortedOdds
 where
  digits = toDigits n

  evens = filter even digits
  odds = filter odd digits

  sortedEvens = sortBy (<) evens
  sortedOdds = sortBy (>) odds
