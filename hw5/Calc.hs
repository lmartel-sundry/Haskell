{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Calc where
import ExprT
import Parser
import qualified Data.Map as M

import Test

-- Helpers
fmap' f Nothing = Nothing
fmap' f (Just x) = Just $ f x

sequence' :: [Maybe a] -> Maybe [a]
sequence' = foldr extract (Just [])
    where extract (Just x) (Just acc) = Just (x:acc)
          extract Nothing _ = Nothing
          extract _ Nothing = Nothing

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = fmap' eval $ parseExp Lit Add Mul s

-- Exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

-- Exercise 4
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = ( > 0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax $ max x y
    mul (MinMax x) (MinMax y) = MinMax $ min x y

instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
    mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

-- Exercise 5
-- See StackVM.hs

-- Exercise 6

class HasVars a where
    var :: String -> a

data VarExprT = VarLit Integer
              | VarAdd VarExprT VarExprT
              | VarMul VarExprT VarExprT
              | Var String
    deriving (Show, Eq)

instance HasVars VarExprT where
    var = Var

instance Expr VarExprT where
    lit = VarLit
    add = VarAdd
    mul = VarMul

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit x _ = Just x
    add f1 f2 m = fmap' sum $ sequence [f1 m, f2 m]
    mul f1 f2 m =  fmap' product $ sequence [f1 m, f2 m]

-- Tests
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

main :: IO ()
main = do
    -- Exercise 1
    assert (eval $ Mul (Add (Lit 2) (Lit 3)) (Lit 4)) ((2+3)*4)

    -- Exercise 2
    assert (evalStr "(2+3)*4") (Just $ (2+3)*4)
    assert (evalStr "(2+3)*") Nothing

    -- Exercise 3
    assert (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT) (Mul (Add (Lit 2) (Lit 3)) (Lit 4))

    -- Exercise 4
    assert (testExp :: Maybe Integer) (Just $ -7)
    assert (testExp :: Maybe Bool) (Just True)
    assert (testExp :: Maybe MinMax) (Just $ MinMax 5)
    assert (testExp :: Maybe Mod7) (Just $ Mod7 0)

    -- Exercise 6
    assert (add (lit 3) (var "x")) (VarAdd (VarLit 3) (Var "x"))
    assert (withVars [("x", 6)] $ add (lit 3) (lit 5)) (Just 8)
    assert (withVars [("x", 6)] $ add (lit 3) (var "x")) (Just 9)
    assert (withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))) (Just 54)

