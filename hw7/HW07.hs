{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Monoid
import Data.Vector (Vector, (!?), (//))

import qualified Data.Vector as V

-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = (return . f) =<< m

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i j v = liftM2 swap (v !? i) (v !? j)
  where swap iv jv = v // [(j, iv), (i, jv)]


-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = pure []
mapM fn (x:xs) = (:) <$> fn x <*> mapM fn xs


getElts :: [Int] -> Vector a -> Maybe [a]
getElts is v = mapM (v !?) is

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v =  (v !?) <$> getRandomR (0, length v - 1)

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec len = sequence $ V.replicate len getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR len rng = sequence $ V.replicate len (getRandomR rng)

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = foldM doSwap v [0..len - 1]
  where len = length v
        chooseSwap :: Int -> Rnd Int
        chooseSwap i = getRandomR (i, len - 1)
        mkSwap :: Vector a -> Int -> Int -> [(Int, a)]
        mkSwap vec i j = [(i, vec V.! j), (j, vec V.! i)]
        doSwap :: Vector a -> Int -> Rnd (Vector a)
        doSwap vec i = (vec //) . mkSwap vec i <$> chooseSwap i


-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v i = (V.filter (< x) v, x, V.filter (> x) v)
  where x = v V.! i

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v
  | null v = v
  | otherwise = qsort [ y | y <- xs, y < x] <> V.cons x (qsort [ y | y <- xs, y >= x])
                where x = V.head v
                      xs = V.tail v


-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v
  | null v = pure v
  | otherwise = pivot >>= merge . partitionAt v
  where pivot = getRandomR (0, length v - 1)
        merge (v1,x,v2) = mappend <$> qsortR v1 <*> (V.cons x <$> qsortR v2)


-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select i v
  | null v = pure Nothing
  | otherwise = pivot >>= branch . partitionAt v
  where pivot = getRandomR (0, length v - 1)
        branch (v1,x,v2)
          | i < i' = select i v1
          | i > i' = select (i - i') v2
          | otherwise = pure $ Just x
          where i' = length v1


-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [Card l s | s <- suits, l <- labels]

newDeck :: Rnd Deck
newDeck =  shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard d
  | null d = Nothing
  | otherwise = Just (V.head d, V.tail d)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards 0 d = Just ([], d)
getCards n d = nextCard d >>= (\(c, d') -> consCard c <$> getCards (n - 1) d')
  where consCard c (cs, d') = (c:cs, d')

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
