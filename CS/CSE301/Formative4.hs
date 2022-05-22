import Data.List
import Data.Maybe
import System.Random
import Control.Monad.State

class Monad m => SelectMonad m where
  select :: [a] -> m a

instance SelectMonad [ ] where
  select xs = xs

instance SelectMonad IO where
  select xs
    | not (null xs) = do i <- getStdRandom (randomR (0, length xs-1))
                         return (xs !! i)
    | otherwise     = error ("cannot select from empty list")

newtype Dist a = Dist { dist :: [(a,Rational)] }  deriving (Show)

instance Monad Dist where
  return x = Dist [(x,1)]
  xm >>= f = Dist [(y,p*q) | (x,p) <- dist xm, (y,q) <- dist (f x)]

instance Functor Dist where
  fmap f xm = xm >>= return . f

instance Applicative Dist where
  pure = return
  xm <*> ym = xm >>= \x -> ym >>= return . x

instance SelectMonad Dist where
  select xs
    | not (null xs) = let n = length xs in Dist [(x, 1 / fromIntegral n) | x <- xs]
    | otherwise     = error ("cannot select from empty list")

code :: SelectMonad m => m Char
code = do
  i <- select [0..3]
  return ("hello" !! i)

prob :: Eq a => Dist a -> a -> Rational
prob xm x = sum [p | (y,p) <- dist xm, x == y]

normalize :: Eq a => Dist a -> Dist a
normalize xm = Dist [(x,prob xm x) | x <- support xm]
  where
    support :: Eq a => Dist a -> [a]
    support xm = nub [x | (x,p) <- dist xm, p > 0]  -- "nub", defined in Data.List, removes duplicates from a list

choose :: SelectMonad m => Int -> [a] -> m [a]
choose 0 xs = return []
choose n xs = 
    do i <- select [0 .. length xs - 1]
       ys <- choose (n - 1) ([xs !! j | j <- [0 .. i - 1]] ++ [xs !! j | j <- [i + 1 .. length xs - 1]])
       return ((xs !! i) : (ys))

simulate :: Monad m => Int -> m Bool -> m Int
simulate 0 f = return 0
simulate n f =
    do
        flag <- f
        ns <- simulate (n - 1) f
        if (flag)
            then return (1 + ns)
            else return ns

data Bin a = L a | B (Bin a) (Bin a)  deriving (Show,Eq)

genTree :: SelectMonad m => [a] -> m (Bin a)
genTree = undefined

type Var = String

data Expr = Num Int | Plus Expr Expr | Times Expr Expr
          | Minus Expr Expr | Div Expr Expr
          | Var Var
  deriving (Show,Read)

data Cmd = Eval Expr | Asn Var Expr
  deriving (Show,Read)

type Store = Var -> Maybe Int

eval1 :: Expr -> State Store Int
eval1 (Num n)       = return n
eval1 (Plus e1 e2)  = do
  n1 <- eval1 e1
  n2 <- eval1 e2
  return (n1 + n2)
eval1 (Minus e1 e2)  = do
  n1 <- eval1 e1
  n2 <- eval1 e2
  return (n1 - n2)
eval1 (Times e1 e2) = do
  n1 <- eval1 e1
  n2 <- eval1 e2
  return (n1 * n2)
eval1 (Div e1 e2) = do
  n1 <- eval1 e1
  n2 <- eval1 e2
  if n2 == 0 then error ("Divide by zero") else return (n1 `div` n2)
eval1 (Var x) = do
    store <- get
    let r = store x
    case r of
        Nothing -> error("error")
        Just x -> return x

update :: (Var,Int) -> Store -> Store
update (x,v) rho = \y -> if y == x then Just v else rho y

run1 :: Cmd -> State Store (Maybe Int)
run1 (Eval expr) = do
    x <- eval1 expr
    return (Just x)
run1 (Asn x expr) = do
    store <- get
    result <- eval1 expr
    let new_store = update (x, result) store
    put new_store
    return Nothing

calc1 :: IO ()
calc1 = go (\_ -> Nothing)
  where
    go :: Store -> IO ()
    go rho = do
      s <- getLine
      let c = read s
      case runState (run1 c) rho of
        (Nothing,rho') -> go rho'
        (Just v,rho')  -> putStrLn (show v) >> go rho'

eval2 :: Expr -> StateT Store (Either String) Int
eval2 = undefined

run2 :: Cmd -> StateT Store (Either String) (Maybe Int)
run2 = undefined

calc2 :: IO ()
calc2 = go (\_ -> Nothing)
  where
    go :: Store -> IO ()
    go rho = do
      s <- getLine
      let c = read s
      case runStateT (run2 c) rho of
        Left err             -> putStrLn ("error: " ++ err) >> go rho
        Right (Nothing,rho') -> go rho'
        Right (Just v,rho')  -> putStrLn (show v) >> go rho'
