module Solutions where

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

-- newtype
