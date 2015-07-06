module Golf where

import Data.List
import Test

-- Exercise 1
skips :: [a] -> [[a]]
skips xs = map (map snd) $ pluck (length xs) (zip [0..] xs)
    where pluck 0 _ = []
          pluck n tuples = pluck (n - 1) tuples ++ [filter (\(index, _) -> (index + 1) `mod` n == 0) tuples]

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima xs = scan xs `intersect` scan (reverse xs)
    where scan list = snd $ foldl rising (head list, []) list
          rising (p, acc) x
            | p < x     = (x, acc ++ [x])
            | otherwise = (x, acc)

-- Exercise 3
histogram :: [Integer] -> String
histogram xs = rows 1 (map (\i -> length $ filter (== i) xs) [0..9]) ++ "==========\n0123456789\n"
    where rows n counts
            | n > maximum counts = ""
            | otherwise          = rows (n + 1) counts ++ map (\c -> if c < n then ' ' else '*') counts ++ "\n"

-- Tests
main :: IO ()
main = do   
    -- Exercise 1
    assert (skips "ABCD") ["ABCD", "BD", "C", "D"]
    assert (skips "hello!") ["hello!", "el!", "l!", "l", "o", "!"]
    assert (skips [1]) ([[1]] :: [[Int]])
    assert (skips [True,False]) [[True,False], [False]]
    assert (skips []) ([] :: [[Int]])
    -- Exercise 2
    assert (localMaxima [2,9,5,6,1]) [9,6]
    assert (localMaxima [2,3,4,1,5]) [4]
    assert (localMaxima [1,2,3,4,5]) []
    assert (localMaxima []) []
    -- Exercise 3
    assert (histogram [3,5]) "   * *    \n==========\n0123456789\n"
    putStr $ histogram [1,1,1,5]
    putStr $ histogram [1,4,5,4,6,6,3,4,2,4,9]
