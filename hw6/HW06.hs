{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
module HW06 where

import Data.List

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n
  | n < 0     = 0
  | otherwise = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
  show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
           ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4 -----------------------------------------

instance Functor Stream where
  fmap fn (Cons x xs) = Cons (fn x) (fmap fn xs)

-- Monoid instance doesn't make sense because "empty stream" for mempty isn't possible

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f $ f x)

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) s2 = Cons x (sInterleave s2 xs)

sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake n (Cons x xs) = x : sTake (n - 1) xs

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+ 1) 0

ruler :: Stream Integer
ruler = rule 0
  where rule n = sInterleave (sRepeat n) (rule $ n + 1)

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand seed = Cons result (rand result)
 where result = (1103515245 * seed + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 221 MB peak -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 155 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x:xs) = Just $ foldl computeMinMax (x,x) xs
  where computeMinMax (mini, maxi) next
          | next < mini = (next, maxi)
          | maxi < next = (mini, next)
          | otherwise   = (mini, maxi)

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532
-- main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
