module Solutions where

import Data.Function (on)

-- типови синоними - не се създава нов тип
type Point = (Double, Double)

-- typedef, using

-- data; pattern matching

isZero :: Int -> Bool
isZero 0 = True
isZero _ = False

-- enum
-- всичките типове в Haskell започват с главна буква
-- Double, Char, Bool
-- [a] -> а е типова променлива
-- [Int]

-- Season се нарича типов конструктор
-- Winter, Spring, Summer, Autumn се наричат конструктори на данни
-- Winter -> 0
-- Spring -> 1
-- Winter < Spring

-- сума на типове
data Season = Winter | Spring | Summer | Autumn
  deriving (Show, Eq, Ord, Enum)

toInt :: Season -> Int
toInt Winter = 0
toInt Spring = 1

-- може да pattern match-ване
-- toString -> show
toString :: Season -> String
toString Winter = "Winter"
toString Spring = "Spring"
toString Summer = "Summer"
toString Autumn = "Autumn"

-- >>> toString Summer

nextSeason :: Season -> Season
nextSeason Autumn = Winter
nextSeason season = succ season

-- ("Ivan", 23)
-- fst, snd

-- struct Person {
-- char *name;
-- unsigned age;
-- char *street;
-- unsigned streetNo;
-- };

-- record-syntax
data Address = Address {street :: String, streetNo :: Int}
  deriving Show

data Person = Person
  -- селектори
  { name :: String
  , age :: Int
  , address :: Address
  }
  deriving Show

-- >>> :t name
-- name :: Person -> String

-- name, age, street, streetNo
data Person2 = Person2 String Int String Int
  deriving (Show, Eq, Ord)

-- char const *getName() const;

alex :: Person
alex = Person "Alex" 21 (Address "abcd" 50)

-- >>> name alex
-- "Alex"

getName :: Person2 -> String
getName (Person2 name age street streetNo) = name

getAge :: Person2 -> Int
getAge (Person2 _ age _ _) = age

data Shape
  = Circle Double
  | Rectangle Double Double
  | Triangle Point Point Point

-- char *toString(int x) {...}
-- char *toString(double x) {...}

prettyPrint :: Shape -> String
-- ad hoc полиморфизъм
prettyPrint (Circle r) = "%%%" ++ "the figure is a circle with a radius of " ++ show r
prettyPrint (Rectangle a b) = "%%%" ++ "the figure is a rectangle with sides " ++ show a ++ " and " ++ show b
prettyPrint _ = "%%%" ++ "foo"

prettyPrint' :: Shape -> String
prettyPrint' shape =
  "%%%" ++ case shape of
    Circle r -> "the figure is a circle with a radius of " ++ show 5
    Rectangle a b -> "rect"
    _ -> "foo"

-- newtype

newtype IntSet = IntSet [Int]

data Ordering2 = LT2 | EQ2 | GT2
  deriving (Show, Eq, Ord, Enum, Bounded)

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy cmp (pivot : xs) = sortBy cmp left ++ [pivot] ++ sortBy cmp right
 where
  -- x < pivot
  -- x `cmp` pivot == LT
  -- (`cmp` pivot) :: a -> Ordering
  -- (== LT) :: Ordering -> Bool
  -- a -> Bool
  left' = filter ((== LT) . (`cmp` pivot)) xs
  left =
    filter
      ( \x -> case x `cmp` pivot of
          LT -> True
          _ -> False
      )
      xs
  -- >= pivot
  -- [1,1,-1,1,6]
  right = filter (\x -> x `cmp` pivot /= LT) xs

data LineSegment = LineSegment {start :: Point, end :: Point}
  deriving (Show, Eq, Ord)

data Shape2
  = Triangle2 Point Point Point
  | Rectangle2 Double Double
  | Circle2 LineSegment

area :: Shape2 -> Double
area (Triangle2 p1 p2 p3) = undefined
area (Rectangle2 a b) = a * b
area (Circle2 (LineSegment (x1, y1) (x2, y2))) = pi * r ^ 2
 where
  dx = x2 - x1
  dy = y2 - y1
  r = sqrt (dx * dx + dy * dy)

sortShapes :: [Shape2] -> [Shape2]
-- (Shape2, Double) -> (Shape2, Double) -> Ordering
sortShapes shapes = map fst $ sortBy (\(s1, a1) (s2, a2) -> compare a1 a2) res1
 where
  areas = map area shapes
  res1 = shapes `zip` areas

-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- Shape2 -> Shape2 -> Ordering
-- 1. (b -> b -> Ordering) ~ (Double -> Double -> Ordering)
-- 2. (Shape2 -> b) ~ (Shape2 -> Double)
-- b ~ Double
sortShapes2 :: [Shape2] -> [Shape2]
sortShapes2 shapes = sortBy (compare `on` area) shapes
