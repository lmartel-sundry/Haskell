-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther xs = skipLast xs where
    skipLast [] = []
    skipLast ys = doubleLast (init ys) ++ [last ys]
    doubleLast [] = []
    doubleLast ys = skipLast (init ys) ++ [2 * last ys]
   
-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ map (sum . toDigits) xs

-- Exercise 4
validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits $ x) `mod` 10 == 0

-- Tests
assert :: (Show a, Eq a) => a -> a -> IO ()
assert got expected
    | got /= expected   = putStrLn $ "TEST FAILED: EXPECTED " ++ show expected ++ ", GOT " ++ show got
    | otherwise         = putStrLn $ "Test passed. Got " ++ show got

main :: IO ()
main = do   
    -- Exercise 1
    assert (toDigits 1234) [1,2,3,4]
    assert (toDigits 0) []
    assert (toDigits (-17)) []
    assert (toDigitsRev 1234) [4,3,2,1]
    -- Exercise 2
    assert (doubleEveryOther [8,7,6,5]) [16,7,12,5]
    assert (doubleEveryOther [1,2,3]) [1,4,3]
    -- Exercise 3
    assert (sumDigits [16,7,12,5]) 22
    assert (sumDigits []) 0
    -- Exercise 4
    assert (validate 4012888888881881) True
    assert (validate 4012888888881882) False

