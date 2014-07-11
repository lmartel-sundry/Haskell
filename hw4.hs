import Test

-- Exercise 1
-- Given:
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
     | even x    = (x - 2) * fun1 xs
     | otherwise = fun1 xs
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2) 
       | otherwise = fun2 (3 * n + 1)

-- Mine:
fun1' :: [Integer] -> Integer
fun1' = foldr (\x z -> z * (x - 2)) 1 . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate hailstone
    where hailstone n
            | even n    = n `div` 2
            | otherwise = 3 * n + 1

-- Exercise 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

treeHeight :: Tree a -> Integer
treeHeight Leaf = -1
treeHeight (Node h _ _ _) = h

foldTree :: [a] -> Tree a
foldTree xs = foldr put Leaf xs
    where count Leaf = 0
          count (Node _ l _ r) = count l + 1 + count r

          height n = floor $ logBase 2 n
        
          put :: a -> Tree a -> Tree a
          put x Leaf = Node 0 Leaf x Leaf
          put x (Node _ l d r)
            | height (count l) <= height (count r)  = Node (1 + (height $ count l + 1)) (put x l) d r
            | otherwise                             = Node (1 + (height $ count r + 1)) l d (put x r)
          
-- TODO

-- Exercise 3
xor :: [Bool] -> Bool
xor = foldr (\b z -> not (z && b) && (z || b)) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- Tests
main :: IO ()
main = do   
    -- Exercise 1
    assert (fun1 []) (fun1' [])
    assert (fun1 [1,3,4,5,6,7]) (fun1' [1,3,4,5,6,7])
    
    assert (fun2 1) (fun2' 1)
    assert (fun2 5) (fun2' 5)
    assert (fun2 11) (fun2' 11)

    -- Exercise 2: Test mostly by eye
    putStrLn . show $ foldTree "AB"
    putStrLn . show $ foldTree "ABCDEFGHIJ"
    assert (treeHeight $ foldTree "ABCDEFGHIJ") 3
    
    -- Exercise 3
    assert (xor [False, True, False]) True
    assert (xor [False, True, False, False, True]) False

    assert (map' (\x -> 2*x) [1,2,3,4]) [2,4,6,8]

