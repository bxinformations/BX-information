module Tests.Tester (test) where

import CNF
import CNF.DIMACS
import CNF.Eval
import Data.List
import Data.Maybe

test1 =
  BigAnd
    [1, 2, 3]
    [ BigOr [Lit 1 True, Lit 2 False],
      BigOr [Lit 2 True, Lit 3 True],
      BigOr [Lit 1 False, Lit 3 False],
      BigOr [Lit 1 False, Lit 2 False, Lit 3 True]
    ]

test2 = BigAnd [1] [BigOr [Lit 1 True, Lit 1 False]]

test3 = BigAnd [3] [BigOr [Lit 3 False], BigOr [Lit 3 True]]

test4 = BigAnd [1, 2] [BigOr [Lit 1 True, Lit 2 True], BigOr [Lit 1 False, Lit 2 False]]

getVarFromSubst :: Subst -> [Var]
getVarFromSubst = foldr (\x -> (++) [fst x]) []

test :: Subst -> CNF -> Bool
test s f = if length (getVarFromSubst s) == length (vars f) then evalCNF s f else error "Lack of Var"
