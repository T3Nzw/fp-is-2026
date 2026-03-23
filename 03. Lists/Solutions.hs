module Solutions where

-- много от задачите имат по-хубави решения
-- с функция от по-висок ред :)

import Prelude hiding
  ( drop
  , head
  , init
  , last
  , product
  , repeat
  , replicate
  , reverse
  , splitAt
  , tail
  , take
  , zip
  )

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

pack :: Eq a => [a] -> [[a]]
pack lst = go [] lst
 where
  go :: Eq a => [a] -> [a] -> [[a]]
  go acc [] = [acc]
  go acc [x] = [x : acc]
  go acc (x : xs@(y : _))
    | x == y = go (x : acc) xs
    | otherwise = (x : acc) : go [] xs

-- >>> pack "aabbbccda"
-- ["aa","bbb","cc","d","a"]

rle :: Eq a => [a] -> [(Int, a)]
rle lst = go (pack lst)
 where
  go :: Eq a => [[a]] -> [(Int, a)]
  go [] = []
  go (x : xs) = (length x, head x) : go xs

-- >>> rle "aabbbccda"
-- [(2,'a'),(3,'b'),(2,'c'),(1,'d'),(1,'a')]

replicate :: Int -> a -> [a]
replicate n x
  | n <= 0 = []
  | otherwise = x : replicate (n - 1) x

-- >>> replicate 4 'a'
-- "aaaa"

decodeRle :: [(Int, a)] -> [a]
decodeRle [] = []
decodeRle ((n, x) : xs) = replicate n x ++ decodeRle xs

-- >>> decodeRle [(2,'a'),(3,'b'),(2,'c'),(1,'d'),(1,'a')]
-- "aabbbccda"

cartesian :: [a] -> [b] -> [(a, b)]
cartesian [] _ = []
cartesian (x : xs) ys = x `pairWithEvery` ys ++ cartesian xs ys
 where
  pairWithEvery :: a -> [b] -> [(a, b)]
  pairWithEvery _ [] = []
  pairWithEvery x (y : ys) = (x, y) : pairWithEvery x ys

-- >>> cartesian [1,2,3] "ab"
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]

cartesian' :: [a] -> [b] -> [(a, b)]
cartesian' xs ys = [(x, y) | x <- xs, y <- ys]

-- >>> cartesian' [1,2,3] "ab"
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x : xs) = let rest = subsets xs in addToEach x rest ++ rest
 where
  addToEach :: a -> [[a]] -> [[a]]
  addToEach _ [] = []
  addToEach x (xs : xss) = (x : xs) : addToEach x xss

-- >>> subsets [1,2,3]
-- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

combinations :: Int -> [a] -> [[a]]
combinations k lst = filterByLength k (subsets lst)
 where
  filterByLength :: Int -> [[a]] -> [[a]]
  filterByLength _ [] = []
  filterByLength k (xs : xss)
    | length xs == k = xs : filterByLength k xss
    | otherwise = filterByLength k xss

-- >>> combinations 2 [1,2,3]
-- [[1,2],[1,3],[2,3]]

isPrefixOf :: Eq a => [a] -> [a] -> Bool
[] `isPrefixOf` _ = True
_ `isPrefixOf` [] = False
(x : xs) `isPrefixOf` (y : ys) = x == y && xs `isPrefixOf` ys

-- >>> [1,2] `isPrefixOf` [1,2,3]
-- True

-- >>> "ab" `isPrefixOf` "ab"
-- True

isSuffixOf :: Eq a => [a] -> [a] -> Bool
suf `isSuffixOf` lst = reverse suf `isPrefixOf` reverse lst

-- >>> "ab" `isSuffixOf` "ab"
-- True

-- >>> "ab" `isSuffixOf` "abc"
-- False
-- списък е инфиксен на друг тстк
-- е префикс на някой суфикс на списъка
isInfixOf :: Eq a => [a] -> [a] -> Bool
inf `isInfixOf` [] = null inf
inf `isInfixOf` lst@(_ : xs) =
  inf `isPrefixOf` lst || inf `isInfixOf` xs

-- >>> "ab" `isInfixOf` "abc"
-- True

-- >>> "bc" `isInfixOf` "abc"
-- True

-- >>> "bc" `isInfixOf` "abcd"
-- True

-- тази ще я оставим така, защото иначе би изглеждала
-- прекалено мизерна (т.е. без функции от по-висок ред :) )
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations lst = concat [map (x :) res | x <- lst, let res = permutations $ filter (/= x) lst]

-- това е неоптимално спрямо паметта :)
repeat :: a -> [a]
repeat x = x : repeat x

-- >>> take 10 (repeat 6)
-- [6,6,6,6,6,6,6,6,6,6]

-- константна памет
repeat' :: a -> [a]
repeat' x = let y = x : y in y

-- >>> take 10 (repeat' 6)
-- [6,6,6,6,6,6,6,6,6,6]

nats :: [Int]
nats = go 0
 where
  go i = i : go (i + 1)

-- >>> take 10 nats
-- [0,1,2,3,4,5,6,7,8,9]

powersOf2 :: [Int]
powersOf2 = go 1
 where
  go :: Int -> [Int]
  go i = i : go (i * 2)

-- >>> take 10 powersOf2
-- [1,2,4,8,16,32,64,128,256,512]

isPrime :: Int -> Bool
isPrime n = hasNoDivisorsIn n [2 .. n - 1]
 where
  hasNoDivisorsIn :: Int -> [Int] -> Bool
  hasNoDivisorsIn _ [] = True
  hasNoDivisorsIn k (x : xs) = k `mod` x /= 0 && hasNoDivisorsIn k xs

-- >>> isPrime 2
-- True

-- >>> isPrime 3
-- True

-- >>> isPrime 4
-- False

primes :: [Int]
primes = go 2
 where
  go :: Int -> [Int]
  go i
    | isPrime i = i : go (i + 1)
    | otherwise = go (i + 1)

-- >>> take 10 primes
-- [2,3,5,7,11,13,17,19,23,29]

fibs :: [Int]
fibs = go 0 1
 where
  go :: Int -> Int -> [Int]
  go prev cur = prev : go cur (prev + cur)

-- >>> take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]
