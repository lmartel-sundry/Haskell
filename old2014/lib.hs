import Test

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x       = x : filter' f xs
    | otherwise = filter' f xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ summary [] = summary
foldl' f summary (x:xs) = foldl' f (summary `f` x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ base [] = base
foldr' f base (x:xs) = x `f` foldr' f base xs

-- Tests
main :: IO ()
main = do   
   assert (map' (\x -> 2*x) [1,2,3,4]) [2,4,6,8]
   assert (filter' (\x -> x `mod` 2 == 0) [1,2,3,4]) [2,4]
   assert (foldl' (+) 5 [1,2,3,4]) 15
   assert (foldr' (+) 5 [1,2,3,4]) 15

