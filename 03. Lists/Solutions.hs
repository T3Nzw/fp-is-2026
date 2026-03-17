module Solutions where

import Prelude hiding (drop, head, init, last, product, reverse, splitAt, tail, take, zip)

-- [1,2,3]
-- 1 : (2 : (3 : []))
-- 1 * (2 * (3 * 1))

product :: [Int] -> Int
product lst
  | null lst = 1
  | otherwise = head lst * product (tail lst)

product2 :: [Int] -> Int
product2 [] = 1
product2 (x : xs) = x * product2 xs

range :: Int -> Int -> [Int]
range a b
  | a > b = []
  | otherwise = a : range (a + 1) b

-- >>> range 1 3
-- [1,2,3]

head :: [a] -> a
head [] = error "empty list"
head (x : _) = x

-- String == [Char]

-- >>> head "abc"
-- 'a'

tail :: [a] -> [a]
tail [] = error "empty list"
tail (_ : xs) = xs

-- [1,2,3]
-- 1 : (2 : (3 : []))
-- 1 : 2 : []
-- [1,2]

init :: [a] -> [a]
init [] = error "empty list"
init [_] = []
init (x : xs) = x : init xs

last :: [a] -> a
last [] = error "empty list"
last [x] = x
last (x : xs) = last xs

butLast :: [a] -> a
butLast [] = error "empty list"
butLast [_] = error "empty list"
-- 1 : 2 : (3 : [])
-- x : y : xs
-- butLast (x : _ : []) = x
butLast [x, _] = x
butLast (x : xs) = butLast xs

append :: [a] -> [a] -> [a]
append [] l2 = l2
append (x : xs) l2 = x : append xs l2

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x : xs) = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop 0 l2 = l2
drop _ [] = []
drop n (x : xs) = drop (n - 1) xs

-- [1,2,3,4,5,6,7,8,9,10]
-- [1] ++ dropEvery [3,4,..]

dropEvery :: Int -> [a] -> [a]
dropEvery _ [] = []
dropEvery n lst
  | n <= 0 = error "n must be positive"
  | otherwise = take (n - 1) lst ++ dropEvery n (drop n lst)

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x l2 = x : l2
insertAt _ x [] = [x]
insertAt n x (y : ys) = y : insertAt (n - 1) x ys

-- [Int], [Double]

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
-- (s,t) :: (a,b)
-- (t,s) :: (b,a)

-- x :: a
-- y :: b
-- (y,x) :: (b,a) /= (a,b)
zip (x : xs) (y : ys) = (x, y) : zip xs ys

-- [[Int]]
splitAt :: Int -> [a] -> ([a], [a])
splitAt n lst = go n [] lst
 where
  go :: Int -> [a] -> [a] -> ([a], [a])
  go 0 acc rest = (acc, rest)
  go n acc (x : xs) = go (n - 1) (acc ++ [x]) xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' 0 l2 = ([], l2)
splitAt' _ [] = ([], [])
splitAt' n (x : xs) =
  let (left, right) = splitAt' (n - 1) xs
   in (x : left, right)

-- lst = x : xs
-- lst :: [a]
-- x :: a
-- xs :: [a]

palindrome :: Eq a => [a] -> Bool
palindrome lst = lst == reverse lst

-- struct Foo {};
-- Foo foo1, foo2; foo1 == foo2;

-- void foo(int); void foo(double); foo('t');

mergeSorted :: Ord a => [a] -> [a] -> [a]
mergeSorted [] l2 = l2
mergeSorted l1 [] = l1
-- [1,2,3]
-- l1 = [1,2,3]
-- x = 1
-- xs = [2,3]
mergeSorted l1@(x : xs) l2@(y : ys)
  | x < y = x : mergeSorted xs l2
  | otherwise = y : mergeSorted l1 ys
