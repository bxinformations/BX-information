import Data.Maybe

-- Exercise 1

-- Q1.1
my_or :: [Bool] -> Bool
my_or = foldr (||) False

-- Q1.2
my_takeWhile :: (a -> Bool) -> [a] -> [a]
my_takeWhile p = foldr (\x acc -> if p x then x : acc else []) ([])

-- Q1.3
my_intersperse :: a -> [a] -> [a]
my_intersperse v = foldr (\x acc -> x : if null acc then [] else v : acc) ([])

-- Q1.4
my_tails :: [a] -> [[a]]
my_tails = foldr (\x y -> (x : (head y)) : y) ([[]])

-- Q1.5
my_isPrefixOf :: Eq a => [a] -> [a] -> Bool
my_isPrefixOf = foldr (\x h -> (\(y) -> if (null y) then False else (h (tail y)) && (x == (head y)))) (\ _ -> True)

-- Exercise 2

data Tree a b = Leaf a | Node b [Tree a b]  deriving (Show,Eq)

foldTree :: (a -> c) -> (b -> [c] -> c) -> Tree a b -> c
foldTree f g (Leaf a)    = f a
foldTree f g (Node b ts) = g b (map (foldTree f g) ts)

-- Q2.1
leaves :: Tree a b -> Int
leaves = foldTree (\x -> 1) (\x y -> sum y)

-- Q2.2
nodes  :: Tree a b -> Int
nodes  = foldTree (\x -> 1) (\x y -> sum y + 1)

-- Q2.3
preorder :: Tree a b -> [Either a b]
preorder = foldTree (\x -> [Left x]) (\x y -> [Right x] ++ (concat y))

-- Exercise 3

-- (Write your proofs as comments below.)
{-
   You can use Haskell's syntax for multiline comments
   if you wish to spread your comments across multiple
   lines without prefixing them by "--"s.
-}
   
-- Q3.1

{-
  Base case:
      reverse ([] ++ ys) 
    = reverse (ys)                      [definition of (++)]
    = [] ++ reverse (ys)                [definition of (++)]
    = reverse ([]) ++ reverse (ys)      [definition of reverse]
  Inductive case:
      reverse ((x : xs) ++ ys)
    = reverse (x : (xs ++ ys))          [definition of (++)]
    = reverse (xs ++ ys) ++ [x]         [definition of reverse]
    = reverse ys ++ reverse xs ++ [x]   [inductive hypothesis]
    = reverse ys ++ (reverse xs ++ [x]) [monoid axioms]
    = reverse ys ++ reverse (x : xs)    [definition of reverse]   ∎
-}

-- Q3.2

{-
  Base case:
      revapp [] ys 
    = ys                                [definition of revapp]
    = [] ++ ys                          [definition of (++)]
    = reverse ([]) ++ ys                [definition of reverse]
  Inductive case:
      revapp (x : xs) ys
    = revapp xs (x : ys)                [definition of revapp]
    = reverse xs ++ (x : ys)            [inductive hypothesis]
    = reverse xs ++ [x] ++ ys           [definition of (:)]
    = reverse xs ++ reverse x ++ ys     [definition of reverse]
    = reverse (x : xs) ++ ys            [Q3.1]   ∎

      reverse xs
    = reverse ([] ++ xs)                [definition of (++)]
    = reverse xs ++ reverse []          [Q3.1]
    = reverse xs ++ []                  [definition of reverse]
    = revap xs []                       [Q3.2]   ∎
-}

-- Exercise 4

type Var = String
data Lit = Lit    { var :: Var , pol :: Bool }    deriving (Show,Eq)
data Cls = BigOr  { literals :: [Lit] }           deriving (Show,Eq)
data Frm = BigAnd { clauses  :: [Cls] }           deriving (Show,Eq)

lem = BigAnd [ BigOr [ Lit "x" True, Lit "x" False ] ]
con = BigAnd [ BigOr [ Lit "x" True ], BigOr [ Lit "x" False ] ]
frm = BigAnd [ BigOr [ Lit "x" True, Lit "y" True ],
               BigOr [ Lit "x" False, Lit "y" False ] ]
fr' = BigAnd [ BigOr [ Lit "x1" True, Lit "x2" True, Lit "x3" False ],
               BigOr [ Lit "x2" True, Lit "x3" True, Lit "x4" False ],
               BigOr [ Lit "x3" True, Lit "x4" True, Lit "x1" True ],
               BigOr [ Lit "x4" True, Lit "x1" False, Lit "x2" True ],
               BigOr [ Lit "x1" False, Lit "x2" False, Lit "x3" True ],
               BigOr [ Lit "x2" False, Lit "x3" False, Lit "x4" True ],
               BigOr [ Lit "x3" False, Lit "x4" False, Lit "x1" False ] ]

type Subst = [(Var,Bool)]

lookupVar :: Subst -> Var -> Bool
lookupVar rho x = fromJust $ lookup x rho

fstL :: Lit -> Var
fstL (Lit var pol) = var

sndL :: Lit -> Bool
sndL (Lit var pol) = pol

-- Q4.1
evalLit :: Subst -> Lit -> Bool
evalLit rho (Lit var pol) = (lookupVar rho var) == pol

-- Q4.2
evalCls :: Subst -> Cls -> Bool
evalCls rho (BigOr c) = or (map (evalLit rho) (c))

-- Q4.3
evalFrm :: Subst -> Frm -> Bool
evalFrm rho (BigAnd f) = and (map (evalCls rho) (f))

-- Q4.4
varsCls :: Cls -> [String]
varsCls (BigOr c) = (foldr (\x acc -> if (fstL x `elem` acc) then acc else (fstL x) : acc) ([])) c

nub :: [String] -> [String]
nub x = (foldr (\y acc -> if (y `elem` acc) then acc else y : acc) []) (x)

varsFrm :: Frm -> [String]
varsFrm (BigAnd f) = (foldr (\x acc -> nub ((varsCls x) ++ acc)) ([])) f

-- Q4.5
substs :: Var -> [Subst] -> [Subst]
substs a as = (foldr (\x acc -> ([(a , True) : x , (a , False) : x] ++ acc)) []) as

allSubsts :: Frm -> [Subst]
allSubsts f = (foldr (\x acc -> substs x acc) [[]]) (varsFrm f)

-- Q4.6
sat :: Frm -> Bool
sat f = or (map (\x -> evalFrm x f) (allSubsts f)) 

-- Q4.7
taut :: Frm -> Bool
taut f = and (map (\x -> evalFrm x f) (allSubsts f)) 

-- Q4.8
solutions :: Frm -> [Subst]
solutions f = (foldr (\x acc -> if (evalFrm x f) then x : acc else acc) ([])) (allSubsts f)

kcolor :: Int -> ([Int],[(Int,Int)]) -> Frm
kcolor k (vs,es) =
   BigAnd $
   [BigOr [ Lit (hascol v c) True | c <- [1..k]] | v <- vs ] ++                             -- every vertex has some color, and
   [BigOr [ Lit (hascol v c) False, Lit (hascol u c) False ] | (v,u) <- es, c <- [1..k] ]   -- no two neighbors have the same color
   where
     hascol :: Int -> Int -> Var
     hascol v c = "hc" ++ show (v,c)

prismGraph = ([1..6],[(1,2),(1,3),(1,4),(2,3),(2,6),(3,5),(4,5),(4,6),(5,6)])

completeGraph n = ([1..n],[(i,j) | i <- [1..n], j <- [1..n], i /= j])

varF :: Var -> Frm
varF x = BigAnd [ BigOr [ Lit x True ] ]

ttF :: Frm
ttF = BigAnd []

ffF :: Frm
ffF = BigAnd [ BigOr [] ]

-- Q4.9
andF :: Frm -> Frm -> Frm
andF (BigAnd f1) (BigAnd f2) = BigAnd (f1 ++ f2)

-- Q4.10

orF  :: Frm -> Frm -> Frm
orF (BigAnd f1) (BigAnd f2) = BigAnd ((foldr (\ (BigOr x) acc -> (((foldr (\ (BigOr y) acc' -> ([BigOr (x ++ y)]) ++ acc') ([])) f1) ++ acc)) ([])) f2)

-- Q4.11
notC :: Cls -> Frm
notC (BigOr c) = BigAnd ((foldr (\x acc -> (BigOr [Lit (fstL x) (not (sndL x))]) : acc) ([])) c)

notF :: Frm -> Frm
notF (BigAnd f) = (foldr (\x acc -> (orF (notC x) (acc))) (ffF)) f

impF :: Frm -> Frm -> Frm
impF p q = notF p `orF` q

iffF :: Frm -> Frm -> Frm
iffF p q = (p `impF` q) `andF` (q `impF` p)

peirce :: Frm
peirce = (((p `impF` q) `impF` p)) `impF` p
  where
    p = varF "p"
    q = varF "q"

-- Q4.12
-- (Test your SAT solver and write your observations as a comment below.)

-- (Since sat use or to calculate the result. Thanks to the Lazy evaluation, when there exists a true, haskell won't calculate
-- the result of other subsitions. So sometime SAT will run fast even if the n is large.)

-- Q4.13
taut' :: Frm -> Bool
taut' = not . sat . notF
-- (Write your observations about taut' vs taut as a comment below.)

--(taut' is slower than taut, since taut' need to perform all the transformations)
