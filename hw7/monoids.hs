{-# LANGUAGE FlexibleInstances #-}
import Data.Monoid

import Test

newtype And = And Bool
    deriving (Eq, Show)

instance Monoid (And) where
    mempty = And True
    mappend (And b1) (And b2) = And (b1 && b2)

newtype Or = Or Bool
    deriving (Eq, Show)

instance Monoid (Or) where
    mempty = Or False
    mappend (Or b1) (Or b2) = Or (b1 || b2)


main :: IO()
main = do
    assert (Or True <> Or False <> mempty) (Or True)
    assert (And True <> And False <> mempty) (And False)

