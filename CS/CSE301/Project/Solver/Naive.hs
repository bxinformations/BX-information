module Solver.Naive (solution) where

import CNF
import CNF.Eval
import Data.List
import Data.Maybe

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = [b : bs | b <- [False, True], bs <- bools (n -1)]

allSubsts :: CNF -> [Subst]
allSubsts f = [zip xs bs | bs <- bools n]
  where
    xs = vars f
    n = length xs

solutions :: CNF -> [Subst]
solutions frm = filter (`evalCNF` frm) (allSubsts frm)

solution :: CNF -> Maybe Subst
solution frm = case solutions frm of
  [] -> Nothing
  (rho : _) -> Just rho
