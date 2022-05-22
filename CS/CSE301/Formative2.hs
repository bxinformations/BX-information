import Data.List
import Data.Maybe
import Control.Monad.STM (check)
import GHC.Stack (popCallStack)

-- Exercise 1

doubleList :: [a] -> [a]
doubleList = foldr (\ x -> (++) [x, x]) []

firstDoubled :: Eq a => [a] -> Maybe a
firstDoubled [] = Nothing
firstDoubled [x] = Nothing
firstDoubled (x : y : xs)
              | x == y = Just x
              | otherwise = firstDoubled (y:xs)

-- Exercise 2

data Allergen = Nuts | Gluten | Soy | Dairy     deriving (Show, Eq)
type Recipe = [Allergen]
data Price = P Int                              deriving (Show, Eq, Ord)
data Cupcake = CC Price Recipe                  deriving (Show, Eq)

getPrice :: Cupcake -> Price
getPrice (CC p r) = p

getRecipe :: Cupcake -> Recipe
getRecipe (CC p r) = r

priceRange :: Price -> Price -> [Cupcake] -> [Cupcake]
priceRange mn mx [] = []
priceRange mn mx (x : xs)
            | mn <= getPrice x && mx >= getPrice x = x : priceRange mn mx xs
            | otherwise = priceRange mn mx xs

test :: [Allergen] -> Recipe -> Bool 
test as [] = True 
test as (x : xs) = (not (x `elem` as)) && test as xs

allergyFree :: [Allergen] -> [Cupcake] -> [Cupcake]
allergyFree as [] = []
allergyFree as (c : cs)
            | test as (getRecipe c) = [c] ++ allergyFree as cs
            | otherwise = allergyFree as cs

-- Exercise 3

type Tin = [Recipe]
data Spec = And Spec Spec | Or Spec Spec | Not Spec | HasCup Int Allergen  deriving (Show,Eq)

sampletin :: Tin
sampletin = [[Nuts], [Dairy,Gluten], [], [Soy]]

checkSpec :: Spec -> Tin -> Bool
checkSpec (And x y) t = (checkSpec x t) && (checkSpec y t)
checkSpec (Or x y) t = (checkSpec x t) || (checkSpec y t)
checkSpec (Not x) t = not (checkSpec x t)
checkSpec (HasCup pos a) t = a `elem` t!!pos

-- Exercise 4

data Tree a b = Leaf a | Node b [Tree a b]  deriving (Show,Eq)

texample :: Tree Char Integer
texample = Node 1 [Node 2 [Leaf 'a', Leaf 'b'], Node 3 [Leaf 'c', Leaf 'd', Leaf 'e'], Node 4 []]

bst :: Tree () Char
bst = Node 'c' [Node 'a' [Leaf (), Node 'b' [Leaf (), Leaf ()]], Node 'd' [Leaf (), Leaf ()]]

preorder :: Tree a b -> [Either a b]
preorder (Leaf a) = [Left a]
preorder (Node b xs) = [Right b] ++ preorders xs
                      where
                        preorders :: [Tree a b] -> [Either a b]
                        preorders [] = []
                        preorders [x] = preorder x
                        preorders (x : xs) = preorder x ++ preorders xs
-- Exercise 5

pop :: Ord a => a -> [a] -> ([a], [a])
pop x [] = ([], [x])
pop x (a : as)
  | x > a = do
              let (pop_pre, stack) = pop x as
              ([a] ++ pop_pre, stack)
  | otherwise = ([], x : a : as)

tmp :: Ord a => [a] -> [a] -> [a]
tmp [] stack = stack
tmp (x : xs) stack = pop_ele ++ (tmp xs (stack_now))
                    where
                      (pop_ele, stack_now) = pop x stack

linearSort :: Ord a => [a] -> [a]
linearSort xs = tmp xs []

-- Exercise 6

counterexample :: [Int]
counterexample = [4, 2, 3, 1]

data Bin = L | B Bin Bin  deriving (Show,Eq)

fromBin :: Bin -> [Int]
fromBin L = [L]
fromBin (B t0 t1) = fromBin t0 ++ [B] ++ fromBin t1

toBin :: [Int] -> Maybe Bin
toBin = undefined
