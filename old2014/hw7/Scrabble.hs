{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where
import Data.Monoid
import Data.Char
import Test

-- Exercise 3

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

instance Monoid Score where
    mappend = (+)
    mempty = Score 0

getScore :: Score -> Int
getScore (Score i) = i

class Scored a where
    score :: a -> Score

instance Scored Score where
    score = id

instance Scored a => Scored (a,b) where
  score = score . fst

instance Scored Char where
    score c
        | isAsciiUpper c = score $ toLower c
        | isAsciiLower c = [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10] !! indexOf c
        | otherwise      = 0
        where indexOf = subtract (ord 'a') . ord

instance Scored String where
    score = Score . sum . map (getScore . score)

scoreString :: String -> Score
scoreString = score
