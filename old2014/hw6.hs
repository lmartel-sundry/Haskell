{-# LANGUAGE FlexibleInstances #-}
import Test

-- Exercise 1
fib :: Integer -> Integer
fib n
    | n <= 0    = 0
    | n == 1    = 1
    | otherwise = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : tail fibs2 `addList` fibs2
    where addList = zipWith (+)

-- Exercise 3
data Stream t = Cons t (Stream t)

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x : streamToList s

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x s) = Cons (f x) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed gen seed = Cons seed (streamFromSeed gen $ gen seed)

-- Exercise 5

streamTail :: Stream a -> Stream a
streamTail (Cons x s) = s

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap twos (streamTail nats)
    where twos n
            | n `mod` 2 == 0    = 1 + twos (n `div` 2)
            | otherwise         = 0

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x s1) s2 = Cons x (streamInterleave s2 s1)

ruler' :: Stream Integer
ruler' = nest 0
    where nest n = streamInterleave (streamRepeat n) (nest $ n + 1)

-- Exercise 6
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0)

    negate (Cons n s) = Cons (-n) (negate s)

    (+) (Cons x0 xs) (Cons y0 ys) = Cons (x0 + y0) (xs + ys)

    (*) (Cons x0 xs) (Cons y0 ys) = Cons (x0 * y0) $ streamMap (*x0) ys + xs * Cons y0 ys

instance Fractional (Stream Integer) where
    (/) (Cons x0 xs) (Cons y0 ys) = q
        where q = Cons (x0 `div` y0) $ streamMap (`div` y0) (xs - q * ys)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2) -- Woah!!

-- Exercise 7
data Row = Row Integer Integer
data Matrix = Matrix Row Row

instance Num Matrix where
    (*) (Matrix (Row a b) (Row c d)) (Matrix (Row e f) (Row g h))
        = Matrix (Row (a*e+b*g) $ a*f+b*h) (Row (c*e+d*g) $ c*f+d*h)

fibs4 :: Stream Integer
fibs4 = Cons 0 $ Cons 1 $ streamMap (\n -> topleft $ m^n) (streamTail nats)
    where m = Matrix (Row 1 1) (Row 1 0)
          topleft (Matrix (Row ul _) _) = ul

main :: IO()
main = do
    -- Exercise 1
    assert (take 15 fibs1) [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]

    -- Exercise 2: manual timing. This should be instant:
    print $ take 30 fibs2

    -- Exercises 3 and 4:
    assert (show $ streamRepeat 5) (show $ replicate 20 5)
    assert (show $ streamMap (\x -> 2*x) $ streamRepeat 5) (show $ replicate 20 10)
    assert (show $ streamFromSeed (+1) 1) (show [1..20])

    -- Exercise 5:
    assert (show ruler) (show [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2])
    assert (show $ streamInterleave (streamRepeat 1) (streamRepeat 0)) (show [1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0])
    assert (show ruler') (show [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2])

    -- Exercise 6:
    assert (show $ x^4) (show [0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
    assert (show $ (1 + x)^5) (show [1,5,10,10,5,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
    assert (show $ (x^2 + x + 3) * (x - 5)) (show [-15,-2,-4,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
    assert (show fibs3) (show $ take 20 fibs2)

    -- Exercise 7:
    assert (show fibs4) (show fibs3)
