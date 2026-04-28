module Solutions where

import Data.Char
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
    (Node 5 Empty Empty)
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
  go q = map getRoot q : go (concat $ map getChildren q)

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
