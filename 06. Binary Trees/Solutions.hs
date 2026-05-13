module Solutions where

import Data.Char
import Data.List (partition)
import GHC.ByteOrder (ByteOrder (BigEndian))
import Prelude hiding (Either (..), Maybe (..))

data BinTree a = Empty | Node a (BinTree a) (BinTree a)
  deriving (Show, Eq, Ord)

foo :: Int -> Bool
foo = undefined

-- foo 5

bt :: BinTree Int
bt =
  Node
    1
    (Node (-2) Empty Empty)
    (Node 3 Empty Empty)

bt2 :: BinTree (Int, Char)
bt2 = undefined

bt3 :: BinTree Int
bt3 =
  Node
    1
    ( Node
        4
        Empty
        (Node 6 Empty Empty)
    )
    ( Node
        3
        (Node 7 Empty Empty)
        (Node 5 Empty Empty)
    )

bt4 :: BinTree Char
bt4 =
  Node
    '1'
    ( Node
        '2'
        Empty
        (Node '3' Empty Empty)
    )
    (Node '4' Empty Empty)

inorder :: BinTree a -> [a]
inorder Empty = []
inorder (Node root left right) =
  inorder right ++ [root] ++ inorder left

data Maybe a = Nothing | Just a
  deriving Show

findEl :: (a -> Bool) -> BinTree a -> Maybe a
findEl p bt =
  case filter p $ inorder bt of
    [] -> Nothing
    (h : _) -> Just h

findEl2 :: (a -> Bool) -> BinTree a -> Maybe a
findEl2 _ Empty = Nothing
findEl2 p (Node root left right) =
  case r of
    Just x -> Just x
    Nothing ->
      if p root
        then Just root
        else findEl2 p left
 where
  r = findEl2 p right

preorder :: BinTree a -> [a]
preorder Empty = []
preorder (Node root left right) =
  [root] ++ preorder left ++ preorder right

-- f :: b -> a

toDigit :: Char -> Int
toDigit c = ord c - ord '0'

-- digitToInt

toInt :: String -> Int
toInt str =
  foldl (\acc digit -> acc * 10 + digitToInt digit) 0 str

-- try/catch

-- 123a52

-- Right -> Just
-- Left  -> Nothing + допълнителна информация
data Either a b = Left a | Right b
  deriving Show

data Error = Error Char
  deriving Show

find :: (a -> Bool) -> [a] -> Maybe a
find p xs = case dropWhile (not . p) xs of
  [] -> Nothing
  (h : _) -> Just h

toNumber :: BinTree Char -> Either Error Int
toNumber bt =
  case find (not . isDigit) digits of
    Nothing -> Right $ toInt digits
    Just x -> Left $ Error x
 where
  digits = preorder bt

level :: Int -> BinTree a -> [a]
level _ Empty = []
level 0 (Node root _ _) = [root]
level n (Node _ left right) = level (n - 1) left ++ level (n - 1) right

height :: BinTree a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)

levels :: BinTree a -> [[a]]
levels t = map (\currLevel -> level currLevel t) [0 .. h - 1]
 where
  h = height t

levels' :: BinTree a -> [[a]]
levels' t = go 0 t
 where
  go :: Int -> BinTree a -> [[a]]
  go n t = case level n t of
    [] -> []
    currLevel -> currLevel : go (n + 1) t

getRoot :: BinTree a -> a
getRoot (Node root _ _) = root
getRoot Empty = error "no elements in the empty tree"

isEmpty :: BinTree a -> Bool
isEmpty Empty = True
isEmpty _ = False

getChildren :: BinTree a -> [BinTree a]
getChildren (Node _ left right) =
  filter (not . isEmpty) [left, right]
getChildren Empty = []

bfs :: BinTree a -> [[a]]
bfs Empty = []
bfs t = go [t]
 where
  go :: [BinTree a] -> [[a]]
  go [] = []
  -- q :: [BinTree a]
  -- getChildren :: BinTree a -> [BinTree a]
  -- map :: (a -> b) -> [a] -> [b]
  -- map getChildren q :: [[BinTree a]]
  -- [BinTree a]
  go q = map getRoot q : go (concatMap getChildren q)

paths :: BinTree a -> [[a]]
paths t = go [] t
 where
  -- [[]]
  go :: [a] -> BinTree a -> [[a]]
  go acc Empty = []
  go acc (Node root Empty Empty) = [acc ++ [root]]
  go acc (Node root left right) =
    let acc' = acc ++ [root]
     in go acc' left ++ go acc' right

isBST :: Ord a => BinTree a -> Bool
isBST Empty = True
isBST (Node x left right) =
  ( case (left, right) of
      (Node y _ _, Node z _ _) -> y <= x && x < z
      (Node y _ _, _) -> y <= x
      (_, Node z _ _) -> x < z
      _ -> True
  )
    && isBST left
    && isBST right

type BST = BinTree

bst1 :: BST Int
bst1 =
  Node
    2
    (Node 1 Empty Empty)
    (Node 3 Empty Empty)

fromList :: Ord a => [a] -> BST a
fromList [] = Empty
fromList (x : xs) = Node x (fromList l) (fromList r)
 where
  (l, r) = partition (<= x) xs

bst2 :: BST Int
bst2 = fromList [5, 2, 61, 6, 6, 2]

bst3 :: BST Int
bst3 = fromList [1, 2, 3]

remove :: Ord a => a -> BST a -> BST a
remove _ Empty = Empty
remove x (Node root left right)
  | x == root =
      if isEmpty left
        then right
        else
          let (newRoot, newTree) = extractMin left in Node newRoot newTree right
  | x < root = Node root (remove x left) right
  | otherwise = Node root left (remove x right)

extractMin :: BST a -> (a, BST a)
extractMin Empty = error "wtf"
extractMin (Node root Empty right) = (root, right)
extractMin (Node root left right) =
  let (min', tree) = extractMin left
   in (min', Node root tree right)

mirrored :: BST a -> BST a
mirrored Empty = Empty
mirrored (Node root left right) =
  Node root (mirrored right) (mirrored left)

isSymmetric :: BST a -> Bool
isSymmetric Empty = True
isSymmetric (Node _ left right) = go left right
 where
  go :: BST a -> BST a -> Bool
  go Empty Empty = True
  go (Node _ l1 r1) (Node _ l2 r2) =
    go l1 r2 && go r1 l2
  go _ _ = False

rightSideView :: BST a -> [a]
rightSideView t = map last $ levels t

rotateLeft :: BBST a -> BBST a
rotateLeft (Node x t1 (Node y t2 t3)) =
  updateHeight $ Node y (updateHeight (Node x t1 t2)) t3
rotateLeft t = t

rotateRight :: BBST a -> BBST a
rotateRight (Node x (Node y t1 t2) t3) =
  updateHeight $ Node y t1 $ updateHeight (Node x t2 t3)

bf :: BST a -> Int
bf Empty = 0
bf (Node _ left right) = height left - height right

isBalanced :: BST a -> Bool
isBalanced Empty = True
isBalanced t@(Node _ left right) =
  abs (bf t) <= 1 && isBalanced left && isBalanced right

type BBST a = BST (Int, a)

getHeight :: BBST a -> Int
getHeight Empty = 0
getHeight (Node (h, _) _ _) = h

updateHeight :: BBST a -> BBST a
updateHeight Empty = Empty
updateHeight (Node (_, root) left right) =
  let hl = getHeight left
      hr = getHeight right
   in Node (1 + max hl hr, root) left right

construct :: BST a -> BBST a
construct Empty = Empty
construct (Node x left right) =
  let left' = construct left
      right' = construct right
   in updateHeight $ Node (undefined, x) left' right'

rotate :: BBST a -> BBST a
rotate t = case bf t of
  -2 -> rotateLeft . (if bf right == 1 then rotateRight else id) $ t
  2 -> rotateRight . (if bf left == -1 then rotateLeft else id) $ t
  _ -> t
 where
  Node _ left right = t

balance :: BBST a -> BBST a
balance Empty = Empty
balance (Node root left right) =
  let left' = balance left
      right' = balance right
      curr' = Node root left' right'
   in updateHeight
        $ if abs (bf curr') <= 1
          then curr'
          else rotate curr'

mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree _ Empty = Empty
mapTree f (Node root left right) =
  Node (f root) (mapTree f left) (mapTree f right)

paths2 :: BinTree a -> [[a]]
paths2 t = go t []
 where
  go :: BinTree a -> [a] -> [[a]]
  go Empty _ = []
  go (Node root Empty Empty) acc =
    [acc ++ [root]]
  go (Node root left right) acc =
    let acc' = acc ++ [root]
     in go left acc' ++ go right acc'

verticalSentence :: String -> BinTree String -> Bool
verticalSentence str bt = str' `elem` paths'
 where
  paths' = paths2 bt
  str' = words str

horizontalSentence :: String -> BinTree String -> Bool
horizontalSentence str bt =
  words str `elem` levels bt

levelsWithEqSums :: BinTree Int -> Maybe (Int, Int)
levelsWithEqSums bt =
  case eqSums of
    [] -> Nothing
    (h : _) -> Just h
 where
  sums = map sum $ levels bt
  indexedSums = [0 ..] `zip` sums

  eqSums =
    [ (m, n)
    | (m, sum1) <- indexedSums
    , (n, sum2) <- indexedSums
    , sum1 == sum2
    , m /= n
    ]
