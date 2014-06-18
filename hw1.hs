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
doubleEveryOther xs = doubleLast (init xs) ++ [last xs] where
    doubleLast [] = []
    doubleLast ys = doubleEveryOther (init ys) ++ [2 * last ys]
   
-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ map (sum . toDigits) xs

-- Exercise 4
validate :: Integer -> Bool
validate x = length (show x) == 16 && (sumDigits . doubleEveryOther . toDigits $ x) `mod` 10 == 0

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ (a, b) : hanoi (n - 1) c b a

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
    assert (validate 0) False
    -- Exercise 5
    assert (hanoi 2 "a" "b" "c") [("a","c"), ("a","b"), ("c","b")]
