module Solutions where

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib' :: Int -> Int
fib' n
  | n < 0 = error "number must be non-negative"
  | n <= 1 = n
  | otherwise = fib' (n - 1) + fib' (n - 2)

sumDigits :: Int -> Int
sumDigits 0 = 0
sumDigits n = n `rem` 10 + sumDigits (n `quot` 10)

pow :: Int -> Int -> Int
pow a 0 = 1
pow a n = a * pow a (n - 1)

quickPow :: Int -> Int -> Int
quickPow a 0 = 1
quickPow a n
  | even n = quickPow (a ^ 2) (n `div` 2)
  | otherwise = a * quickPow a (n - 1)
