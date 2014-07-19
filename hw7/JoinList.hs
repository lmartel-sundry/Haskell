{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where
import Data.Monoid
import Sized
import Buffer
import Editor

import Test
import Scrabble

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Exercise 1

tag :: Monoid m => JoinList m a -> m
tag (Append m _ _) = m
tag (Single m _) = m
tag Empty = mempty

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

-- Exercise 2

pullSize :: (Sized m, Monoid m) => JoinList m a -> Int
pullSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ x) = Just x
indexJ _ (Single _ _) = Nothing
indexJ i (Append m l r)
    | diff < 0  = indexJ i l
    | otherwise = indexJ diff r
    where diff = i - pullSize l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | n <= 0 = jl
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty
dropJ n (Append m l r) = Append m (dropJ n l) (dropJ diff r)
    where diff = n - pullSize r

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0 = Empty
takeJ _ Empty = Empty
takeJ _ jl@(Single _ _) = jl
takeJ n (Append m l r) = Append m (takeJ n l) (takeJ diff r)
    where diff = n - pullSize r

-- Exercise 3 Testing (see Scrabble.hs for Ex. 3)
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Exercise 4

pullScore :: (Scored m, Monoid m) => JoinList m a -> Int
pullScore = getScore . score . tag

instance Buffer (JoinList (Score, Size) String) where
    toString = concat . jlToList
    fromString s = Single (scoreString s, Size 1) s
    line = indexJ
    replaceLine n s b = takeJ (n-1) b +++ fromString s +++ dropJ n b
    numLines = pullSize
    value = pullScore


main :: IO()
main = do
        -- Exercise 1
        assert (Single (Product 2) 'e' +++ Single (Product 3) 'a') simple

        -- Exercise 2
        assert (indexJ 0 sized) (jlToList sized !!? 0)
        assert (indexJ 1 sized) (jlToList sized !!? 1)
        assert (indexJ 2 sized) (jlToList sized !!? 2)

        assert (jlToList $ dropJ 0 sized) (jlToList sized)
        assert (jlToList $ dropJ 1 sized) (drop 1 $ jlToList sized)
        assert (jlToList $ dropJ 2 sized) (drop 2 $ jlToList sized)

        assert (jlToList $ takeJ 0 sized) []
        assert (jlToList $ takeJ 1 sized) (take 1 $ jlToList sized)
        assert (jlToList $ takeJ 2 sized) (take 2 $ jlToList sized)

        -- Exercise 3
        assert (Score 5 <> Score 2 <> mempty) (Score 7)
        assert (scoreLine "yay " +++ scoreLine "haskell!") scored

        -- Exercise 4
        -- TODO TEST EXERCISE 4

    where simple = Append (Product 6) (Single (Product 2) 'e') (Single (Product 3) 'a')
          sized = Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'a')
          scored = Append (Score 23) (Single (Score 9) "yay ") (Single (Score 14) "haskell!")

