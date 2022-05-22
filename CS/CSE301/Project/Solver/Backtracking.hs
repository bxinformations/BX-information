module Solver.Backtracking (solution) where

import CNF
import CNF.Eval
import Data.List
import Data.Maybe
import Language.Haskell.TH (litE)

test =
  [ BigOr [Lit 1 True, Lit 2 False],
    BigOr [Lit 2 True, Lit 3 True],
    BigOr [Lit 1 False, Lit 3 False],
    BigOr [Lit 1 False, Lit 2 False, Lit 3 True]
  ]

test2 = [BigOr [Lit 1 True, Lit 1 False]]

test3 = [BigOr [Lit 3 False], BigOr [Lit 3 True]]

test4 = [BigOr [Lit 1 True, Lit 2 True], BigOr [Lit 1 False, Lit 2 False]]

opp :: Lit -> Lit
opp (Lit v True) = Lit v False
opp (Lit v False) = Lit v True

fstL :: Lit -> Var
fstL (Lit var pol) = var

sndL :: Lit -> Bool
sndL (Lit var pol) = pol

giveSub :: [Var] -> Subst
giveSub = foldr (\x -> (++) [(x, True)]) []

varInCls :: Lit -> [Lit] -> Cls -> Maybe Cls
varInCls v now (BigOr []) = Just (BigOr now)
varInCls v now (BigOr (x : xs)) =
  if fst (unLit x) == fst (unLit v)
    then (if snd (unLit x) == snd (unLit v) then Nothing else varInCls v now (BigOr xs))
    else varInCls v (now ++ [x]) (BigOr xs)

condition :: Lit -> [Cls] -> Maybe ([Cls], Maybe Lit)
condition v [] = Nothing
condition v (x : xs) = case varInCls v [] x of
  Nothing -> condition v xs
  Just k ->
    if null (literals k)
      then Just ([], Nothing)
      else
        ( case condition v xs of
            Nothing -> case length (literals k) of
              1 -> Just ([k], Just (head (literals k)))
              _ -> Just ([k], Nothing)
            Just (ks, Nothing) -> case ks of
              [] -> Just ([], Nothing)
              z : zs -> case length (literals k) of
                1 -> Just (k : (z : zs), Just (head (literals k)))
                _ -> Just (k : (z : zs), Nothing)
            Just (ks, lit) -> case ks of
              [] -> Just ([], Nothing)
              z : zs -> Just (k : (z : zs), lit)
        )

findVariable :: Cls -> Maybe Lit
findVariable (BigOr f) = case f of
  [] -> Nothing
  x : _ -> Just x

mostCommon :: [Lit] -> Lit
mostCommon s = snd $maximum [([1 | y <- s, y == x], x) | x <- s]

findVariableHeu :: [Cls] -> Maybe Lit
findVariableHeu cls = findVariableHeuAux (litFrm cls)
  where
    findVariableHeuAux :: [Lit] -> Maybe Lit
    findVariableHeuAux lits = case lits of
      [] -> Nothing
      l : lits -> Just (mostCommon (l : lits))

findSingleLit :: [Cls] -> Maybe Lit
findSingleLit [] = Nothing
findSingleLit (y : ys) = case length (literals y) of
  1 -> Just (head (literals y))
  _ -> findSingleLit ys

findPureLit :: [Cls] -> Maybe Lit
findPureLit cls = findPureLitAux (litFrm cls)
  where
    findPureLitAux :: [Lit] -> Maybe Lit
    findPureLitAux [] = Nothing
    findPureLitAux [l] = Just l
    findPureLitAux (l1 : l2 : lts) =
      if var l1 == var l2
        then findPureLitAux lts
        else Just l1

solve :: [Cls] -> Subst -> Maybe Lit -> Maybe Subst
solve [] s _ = Just []
solve (x : xs) s (Just single) = case condition single (x : xs) of
  Nothing -> Just (unLit single : s)
  Just (k, new_single) -> case k of
    [] -> Nothing
    y : ys -> solve (y : ys) (unLit single : s) new_single
solve (x : xs) s Nothing = case findPureLit (x : xs) of
  Nothing -> case findVariable x of
    Nothing -> Nothing
    Just v -> case condition v (x : xs) of
      Nothing -> Just (unLit v : s)
      Just (k, new_single) -> case k of
        [] -> case condition (opp v) (x : xs) of
          Nothing -> Just (unLit (opp v) : s)
          Just (k, new_single1) -> case k of
            [] -> Nothing
            y : ys -> solve (y : ys) (unLit (opp v) : s) new_single1
        y : ys -> case solve (y : ys) (unLit v : s) new_single of
          Nothing -> case condition (opp v) (x : xs) of
            Nothing -> Just (unLit (opp v) : s)
            Just (k, new_single1) -> case k of
              [] -> Nothing
              z : zs -> solve (z : zs) (unLit (opp v) : s) new_single1
          Just s -> Just s
  Just l -> case condition l (x : xs) of
    Nothing -> Just (unLit l : s)
    Just (k, new_single) -> case k of
      [] -> Nothing
      y : ys -> solve (y : ys) (unLit l : s) new_single

getVarFromSubst :: Subst -> [Var]
getVarFromSubst = map fst

contain :: [Lit] -> [Lit] -> Bool
contain xs = foldr (\y -> (&&) (y `elem` xs)) True

containC :: [Lit] -> [Cls] -> Bool
containC x = foldr ((||) . (\y -> contain y x) . literals) False

-- if Cls1 \in Cls2 then we can delete Cls2
cancelImply :: [Cls] -> [Cls]
cancelImply [] = []
cancelImply (x : xs) = if containC (literals x) xs then cancelImply xs else x : cancelImply xs

solution :: CNF -> Maybe Subst
solution f = case solve fc [] (findSingleLit (clauses f)) of
  Nothing -> Nothing
  Just s -> Just (s ++ giveSub (vars f \\ getVarFromSubst s))
  where
    -- fc = sortBy (\e1 e2 -> compare (length (literals e1)) (length (literals e2))) (clauses f)
    fc = sortBy (\e1 e2 -> compare (length (literals e2)) (length (literals e1))) (clauses f)

-- fc = cancelImply (clauses f) -- slowing down
--fc = clauses f