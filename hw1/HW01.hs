{-# OPTIONS_GHC -Wall #-}
module HW01 where

import Test
import Data.Char (digitToInt)

-- Helpers
digits :: Integer -> [Integer]
digits x
  | x <= 0    = []
  | otherwise = (map $ toInteger . digitToInt) . show $ x

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits = reverse . digits

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs) = x : double xs
  where double (y:ys) = (2 * y) : doubleEveryOther ys
        double [] = []

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = sumUp . map (sumUp . digits)
  where sumUp xs = foldl (+) 0 xs


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn 0 = False
luhn x = (== 0) . lastDigit . sumDigits . doubleEveryOther . toRevDigits $ x

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

-- Tests
main :: IO ()
main = do
    -- Exercise 1
    assert (digits 1234) [1,2,3,4]
    assert (digits 0) []
    assert (digits (-17)) []
    assert (reverse . digits $ 1234) [4,3,2,1]
    -- Exercise 2
    assert (doubleEveryOther [8,7,6,5]) [8,14,6,10]
    assert (doubleEveryOther [1,2,3]) [1,4,3]
    -- Exercise 3
    assert (sumDigits [16,7,12,5]) 22
    assert (sumDigits []) 0
    -- Exercise 4
    assert (luhn 4012888888881881) True
    assert (luhn 4012888888881882) False
    assert (luhn 0) False
    -- Exercise 5
    assert (hanoi 2 "a" "b" "c") [("a","c"), ("a","b"), ("c","b")]
