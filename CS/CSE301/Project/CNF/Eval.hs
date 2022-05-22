-- evaluation of CNF formulas under a substitution
module CNF.Eval where

import CNF

evalLit :: Subst -> Lit -> Bool
evalCls :: Subst -> Cls -> Bool
evalCNF :: Subst -> CNF -> Bool
lookupVar :: Subst -> Var -> Bool
lookupVar rho x = case lookup x rho of
  Nothing -> error ("lookupVar: " ++ show x ++ " not in substitution")
  Just b -> b
evalLit rho x = (if pol x then id else not) $ lookupVar rho (var x)

evalCls rho = any (evalLit rho) . literals

evalCNF rho = all (evalCls rho) . clauses
