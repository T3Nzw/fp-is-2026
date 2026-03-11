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

reverseNumber :: Int -> Int
reverseNumber n = reverseHelper n 0
 where
  reverseHelper :: Int -> Int -> Int
  reverseHelper 0 acc = acc
  reverseHelper n acc = reverseHelper (n `div` 10) (acc * 10 + n `mod` 10)

palindrome :: Int -> Bool
palindrome n = n == reverseNumber n

-- lcm' :: Int -> Int -> Int
-- lcm' a b = lcmHelper (a * b) (min a b) (max a b)
-- where
-- lcmHelper prod min' max'
-- \| prod `mod` max' /= 0 = prod * min'
-- \| otherwise = lcmHelper (prod `div` min') min' max'

lcm' :: Int -> Int -> Int
lcm' a b =
  let maxAB = max a b
      minAB = min a b
   in lcmHelper maxAB minAB maxAB
 where
  lcmHelper :: Int -> Int -> Int -> Int
  lcmHelper acc min' max'
    | acc `mod` max' == 0 && acc `mod` min' == 0 = acc
    | otherwise = lcmHelper (acc * min') min' max'

gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' 0 b = b
gcd' a b
  | a > b = gcd' (a `mod` b) b
  | otherwise = gcd' a (b `mod` a)

lcm'' :: Int -> Int -> Int
lcm'' a b = a * b `div` gcd' a b

succ' :: Int -> Int
succ' n = n + 1

pred' :: Int -> Int
pred' 0 = 0
pred' n = predHelper n 0
 where
  predHelper :: Int -> Int -> Int
  predHelper n k
    | n < 0 = error "n must be non-negative"
    | n == succ' k = k
    | otherwise = predHelper n (succ' k)

add' :: Int -> Int -> Int
add' 0 n = n
add' m n = add' (pred' m) (succ' n)

mul' :: Int -> Int -> Int
mul' 0 _ = 0
mul' m n = add' n (mul' (pred' m) n)

ack :: Int -> Int -> Int
ack 0 n = n + 1
ack m 0 = ack (m - 1) 1
ack m n = ack (m - 1) (ack m (n - 1))

{-
int x = 10;

int main() {
  x = 15;
}
-}

kaprekar :: Int -> Bool
kaprekar x = helper (x ^ 2) 0 1
 where
  helper :: Int -> Int -> Int -> Bool
  helper lhs rhs i
    | lhs == 0 = False
    | lhs + rhs == x = True
    | otherwise = helper (lhs `div` 10) (lhs `mod` 10 * i + rhs) (i * 10)

prime :: Int -> Bool
prime n = n > 1 && go n 2
 where
  go n k
    | k >= n = True
    | otherwise = n `mod` k /= 0 && go n (k + 1)

smallestPrimeDivisor :: Int -> Int
smallestPrimeDivisor n = go n 2
 where
  go :: Int -> Int -> Int
  go n k
    | n `mod` k == 0 && prime k = k
    | otherwise = go n (k + 1)

sumDig :: Int -> Int
sumDig n = sumDigits (smallestPrimeDivisor n)

-- >>> sumDig 121
-- 2
