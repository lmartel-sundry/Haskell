{-# OPTIONS_GHC -Wall #-}
module HW04 where
import Data.List
import Test


newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
  P xs == P ys = shave xs == shave ys
    where shave = reverse . trim . reverse
          trim (0:zs) = trim zs
          trim zs = zs

-- Exercise 3 -----------------------------------------

zipWithIndex :: [a] -> [(a, Int)]
zipWithIndex xs = zip xs [0..]

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P []) = "0"
    show (P cs) = (intercalate " + ") . (filter (/= "")) . (map showTerm) . reverse . zipWithIndex $ cs
      where showTerm :: (Num c, Eq c, Show c) => (c, Int) -> String
            showTerm (c, e)
              | c == 0 = ""
              | e == 0 = show c
              | c == -1 = "-" ++ showExp e
              | c == 1 = showExp e
              | otherwise = show c ++ showExp e
            showExp e
              | e == 1 = "x"
              | otherwise = "x^" ++ show e

-- Exercise 4 -----------------------------------------

zipWithDefaults :: a -> b -> [a] -> [b] -> [(a,b)]
zipWithDefaults dx dy (x':xs) (y:ys) = (x',y) : zipWithDefaults dx dy xs ys
zipWithDefaults dx _ [] ys = zip (repeat dx) ys
zipWithDefaults _ dy xs [] = zip xs (repeat dy)

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P cs) (P ds) = P . (map (\(a,b) -> a + b)) $ zipWithDefaults zero zero cs ds
  where zero = fromInteger 0

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P cs) (P ds) = sum . (map P) . (map shiftBy) . zipWithIndex $ map (scaleBy ds) cs
  where scaleBy (x':xs) c = (c * x') : scaleBy xs c
        scaleBy [] _ = []
        shiftBy :: Num a => ([a], Int) -> [a]
        shiftBy (xs, n) = (replicate n 0) ++ xs

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P cs) = P (map negate cs)
    fromInteger i = P [fromInteger i]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P cs) v = sum . map computeTerm $ zipWithIndex cs
  where computeTerm (c, e) = c * v^e


-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n = (!! n) . (iterate deriv)

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P cs) = P . tail . map scaleByExp $ zipWithIndex cs
      where scaleByExp (c,e) = c * (fromIntegral e)

-- Tests

poly :: [Integer] -> Poly Integer
poly = P

main :: IO ()
main = do
    -- Exercise 2
    assert (poly [1, 2, 3] == poly [1, 2, 3]) True
    assert (poly [1, 2] /= poly [1, 2, 3]) True
    assert (poly [1, 2, 3, 0, 0] == poly [1, 2, 3]) True
    assert (poly [1, 0, 3] == poly [1, 3]) False
    -- Exercise 3
    assert (show . poly $ [1, 0, 0, 2]) "2x^3 + 1"
    assert (show . poly $ [0, -1, 2]) "2x^2 + -x"
    -- Exercise 4
    assert (poly [5, 0, 1] + poly [1, 1, 2]) (poly [6, 1, 3])
    assert (poly [1, 0, 1] + poly [1, 1]) (poly [2, 1, 1])
    -- Exercise 5
    assert (poly [1, 1, 1] * poly [2, 2]) (poly [2, 4, 4, 2])
    -- Exercise 6
    assert (negate $ poly [1, 2, 3]) (poly [-1, -2, -3])
    assert (poly [3, 5, 1]) (x^2 + 5*x + 3)
    -- Exercise 7
    assert (applyP (x^2 + 2*x + 1) 1) 4
    assert (applyP (x^2 + 2*x + 1) 2) 9
    -- Exercise 8, 9
    assert (deriv $ x^2 + 3*x + 5) (2*x + 3)
    assert (nderiv 2 $ x^2 + 3*x + 5) (P [2])
