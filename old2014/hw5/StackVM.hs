{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module StackVM (StackVal(..), StackExp(..), Stack, Program, stackVM) where
import Test
import Parser

-- Values that may appear in the stack. Such a value will also be
-- returned by the stackVM program execution function.
data StackVal = IVal Integer | BVal Bool | Void deriving (Show, Eq)

-- The various expressions our VM understands.
data StackExp = PushI Integer
              | PushB Bool
              | Add
              | Mul
              | And
              | Or
                deriving (Show, Eq)

type Stack   = [StackVal]
type Program = [StackExp]

-- Execute the given program. Returns either an error message or the
-- value on top of the stack after execution.
stackVM :: Program -> Either String StackVal
stackVM = execute []

errType :: String -> Either String a
errType op = Left $ "Encountered '" ++ op ++ "' opcode with ill-typed stack."

errUnderflow :: String -> Either String a
errUnderflow op = Left $ "Stack underflow with '" ++ op ++ "' opcode."

-- Execute a program against a given stack.
execute :: Stack -> Program -> Either String StackVal
execute [] []                               = Right Void
execute (s:_) []                            = Right s

execute s (PushI x : xs)                    = execute (IVal x : s) xs
execute s (PushB x : xs)                    = execute (BVal x : s) xs

execute (IVal s1 : IVal s2 : ss) (Add : xs) = execute (s':ss) xs
    where s' = IVal (s1 + s2)
execute (_:_:_) (Add:_)                     = errType "Add"
execute _ (Add:_)                           = errUnderflow "Add"

execute (IVal s1:IVal s2:ss) (Mul : xs)     = execute (s':ss) xs
    where s' = IVal (s1 * s2)
execute (_:_:_) (Mul:_)                     = errType "Mul"
execute _ (Mul:_)                           = errUnderflow "Mul"

execute (BVal s1:BVal s2:ss) (And : xs)     = execute (s':ss) xs
    where s' = BVal (s1 && s2)
execute (_:_:_) (And:_)                     = errType "And"
execute _ (And:_)                           = errUnderflow "And"

execute (BVal s1 : BVal s2 : ss) (Or : xs)  = execute (s':ss) xs
    where s' = BVal (s1 || s2)
execute (_:_:_) (Or:_)                      = errType "Or"
execute _ (Or:_)                            = errUnderflow "Or"

test = stackVM [PushI 3, PushI 5, Add]

-- Exercise 5
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Program where
    lit x = [PushI x]
    add p1 p2 = p1 ++ p2 ++ [Add]
    mul p1 p2 = p1 ++ p2 ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

-- Tests

-- check :: Expr a => a -> Bool
-- check exp = stackVM exp == Right (IVal exp)

main :: IO()
main = do
    -- Exercise 5
    assert (mul (add (lit 2) (lit 3)) (lit 4) :: Program) [PushI 2, PushI 3, Add, PushI 4, Mul]
    assert (stackVM $ mul (add (lit 2) (lit 3)) (lit 4)) (Right . IVal $ mul (add (lit 2) (lit 3)) (lit 4))

