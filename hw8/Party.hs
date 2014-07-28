{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where
import Control.Applicative
import Data.List
import Data.Monoid
import Data.Tree
import Employee
import Test

-- Exericse 1
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) (empFun e + fun)

instance Monoid GuestList where
    mappend (GL es1 fun1) (GL es2 fun2) = GL (es1 ++ es2) (fun1 + fun2)
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ fun1) gl2@(GL _ fun2)
    | fun1 < fun2 = gl2
    | otherwise   = gl1

-- Exercise 2
-- treeFold :: (a -> b) -> (a -> [b] -> b) -> Tree a -> b
-- treeFold labelFn _ (Node label []) = labelFn label
-- treeFold labelFn combFn (Node label children) = combFn label (map (treeFold labelFn combFn) children)

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold combFn (Node label children) = combFn label $ map (treeFold combFn) children

-- Exercise 3
-- combineGLs :: Employee -> [GuestList] -> GuestList
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel root childLists = (withRoot childLists, withoutRoot childLists)
    where -- if we invite the root, we cannot invite any immediate children
          withRoot :: [(GuestList, GuestList)] -> GuestList
          withRoot = glCons root . mconcat . map snd
          -- without inviting the root, we can choose either child list
          withoutRoot :: [(GuestList, GuestList)] -> GuestList
          withoutRoot = mconcat . map (uncurry moreFun)


-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- Tests
glFun :: GuestList -> Fun
glFun (GL _ fun) = fun

glGuests :: GuestList -> [Employee]
glGuests (GL es _) = es

main :: IO()
main = do
        -- Exercise 1
        assert (glCons bob . glCons alice $ GL [] 0 ) (GL [bob, alice] 3)
        assert (glCons bob mempty <> glCons alice mempty) (GL [bob, alice] 3)
        assert (moreFun (glCons bob mempty) (glCons alice mempty)) (glCons alice mempty)

        -- Exercise 4
        assert (glFun $ maxFun testCompany) 26
        assert (glFun $ maxFun testCompany2) 26

        -- Exercise 5
        putStrLn "===Begin Exercise 5 Output==="
        -- readFile "company.txt" >>= (print . glFun . maxFun . read)
        gl <- maxFun . read <$> readFile "company.txt"
        putStrLn $ "Total fun: " ++ (show . glFun) gl
        putStrLn . intercalate "\n" . sort . map empName $ glGuests gl

    where alice = Emp { empName = "Alice", empFun = 2 }
          bob = Emp { empName = "Bob", empFun = 1 }
