-- A small collection of problems for testing a SAT solver
module CNF.Problems where

import CNF (CNF (..), Cls (BigOr), Lit (Lit), Var, varsFrm)

(x, y, z) = (1, 2, 3)

-- some small CNFs
lem = BigAnd [1 .. 1] [BigOr [Lit x True, Lit x False]]

con = BigAnd [1 .. 1] [BigOr [Lit x True], BigOr [Lit x False]]

frm =
  BigAnd
    [1 .. 2]
    [ BigOr [Lit x True, Lit y True],
      BigOr [Lit x False, Lit y False]
    ]

fr' =
  BigAnd
    [1 .. 4]
    [ BigOr [Lit 1 True, Lit 2 True, Lit 3 False],
      BigOr [Lit 2 True, Lit 3 True, Lit 4 False],
      BigOr [Lit 3 True, Lit 4 True, Lit 1 True],
      BigOr [Lit 4 True, Lit 1 False, Lit 2 True],
      BigOr [Lit 1 False, Lit 2 False, Lit 3 True],
      BigOr [Lit 2 False, Lit 3 False, Lit 4 True],
      BigOr [Lit 3 False, Lit 4 False, Lit 1 False]
    ]

-- A family of simple tautologies
dichotomy :: Int -> CNF
dichotomy n = BigAnd [1 .. n] [BigOr [Lit i True, Lit i False] | i <- [1 .. n]]

-- For example, dichotomy 3 stands for the valid formula (x1 | ~x1) & (x2 | ~x2) & (x3 | ~x3)

-- CNFs derived from graph-coloring problems
type Graph = ([Int], [(Int, Int)])

kcolor :: Int -> Graph -> CNF
kcolor k (vs, es) =
  BigAnd [1 .. k * length vs] $
    [BigOr [Lit (hascol v c) True | c <- [1 .. k]] | v <- vs]
      ++ [BigOr [Lit (hascol v c) False, Lit (hascol u c) False] | (v, u) <- es, c <- [1 .. k]]
  where
    hascol :: Int -> Int -> Var
    hascol v c = (v -1) * k + c -- U ("hc" ++ show (v,c))

prismGraph :: Graph
prismGraph = ([1 .. 6], [(1, 2), (1, 3), (1, 4), (2, 3), (2, 6), (3, 5), (4, 5), (4, 6), (5, 6)])

completeGraph :: Int -> Graph
completeGraph n = ([1 .. n], [(i, j) | i <- [1 .. n], j <- [1 .. n], i /= j])

color_prismGraph :: CNF
color_prismGraph = kcolor 3 prismGraph

color_completeGraph :: Int -> Int -> CNF
color_completeGraph n k = kcolor k (completeGraph n)

kcolorCLS :: Int -> Graph -> [Cls]
kcolorCLS k (vs, es) = [BigOr [Lit (hascol v c) True | c <- [1 .. k]] | v <- vs] ++ [BigOr [Lit (hascol v c) False, Lit (hascol u c) False] | (v, u) <- es, c <- [1 .. k]]
  where
    hascol :: Int -> Int -> Var
    hascol v c = 10 * v + c

kcolorOurV :: Int -> Int -> CNF
kcolorOurV n m = BigAnd {vars = vars, clauses = cls}
  where
    cls = kcolorCLS n (completeGraph m)
    vars = varsFrm cls

-- Propositional pigeon-hole principle: php m n states that m pigeons
-- can be placed in n holes without two pigeons sharing the same hole.
-- The formula is satisfiable iff m <= n, but note that checking that
-- formula php (n+1) n is unsatisfiable is notoriously hard for SAT solvers
-- even for small values of n!
php :: Int -> Int -> CNF
php m n =
  BigAnd [1 .. m * n] $
    [BigOr [Lit (pigeon p h) True | h <- [1 .. n]] | p <- [1 .. m]]
      ++ [BigOr [Lit (pigeon p1 h) False, Lit (pigeon p2 h) False] | p1 <- [1 .. m], p2 <- [1 .. p1 -1], h <- [1 .. n]]
  where
    pigeon :: Int -> Int -> Var
    pigeon p h = (p -1) * n + h
