module Main where

import Data.List (intercalate)

ex1 :: IO ()
ex1 = putStrLn "i love fmi"

main :: IO ()
main = undefined

data Void

-- data () = ()

data Unit = Unit

-- IO a -> a

fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x
fromMaybe _ = error "njkfn"

-- putStrLn, print, getLine

-- std::cin.getline(str, N);
-- std::cout << str;

ex2 :: IO ()
ex2 = getLine >>= putStrLn

ex22 :: IO ()
ex22 = do
  -- getLine :: IO String
  -- str :: String
  str <- getLine
  putStrLn str :: IO ()

-- putStrLn :: String -> IO ()
-- putStrLn "abcd" :: IO ()

-- getLine :: IO String
-- putStrLn :: String -> IO ()

-- (>>=) :: IO a -> (a -> IO b) -> IO b

readInt :: IO Int
readInt = do
  str :: String <- getLine
  return (read str :: Int)

{-
unsigned n;
std::cin >> n;

for (...) {
  int x;
  std::cin >> x;
  std::cout << x;
}
-}

-- void foo() {}

printNumbers :: IO ()
printNumbers = do
  n <- readInt
  helper n
 where
  helper :: Int -> IO ()
  helper 0 = return ()
  helper n = do
    x <- readInt
    print x
    helper (n - 1)

readInts :: IO [Int]
readInts = do
  n <- readInt
  helper n
 where
  -- n = 2
  -- x = 1
  -- x = 2
  -- n:0 => []

  -- 1 : 2 :[]

  helper :: Int -> IO [Int]
  helper 0 = return []
  helper n = do
    x :: Int <- readInt
    xs :: [Int] <- helper (n - 1) :: IO [Int]
    return $ x : xs

-- x : helper (n - 1)

printNumbers2 :: IO ()
printNumbers2 = do
  is :: [Int] <- readInts :: IO [Int]
  let str = unwords $ map show is
  return str >>= putStrLn

-- return str :: IO String
-- putStrLn :: String -> IO ()
