module Formative3 where

-- import System.Random hiding (uniform)

-- Exercise 1

{-
fun1 x = [x]                        a -> [a]

fun2 x = x []                       ([c] -> b) -> b

fun3 x y z = x z y                  (a -> b -> c) -> b -> a -> c

fun4 x y = x (x (x (x y)))          (a -> a) -> a -> a

fun5 x y = x y x                    This is not well typed.
-}

-- Exercise 2

{-
mysteryfun1 :: (a -> b -> c) -> (b -> a -> c)

mysteryfun2 :: (a -> a -> b) -> (a -> a -> b)
-}

-- (write your observations as a comment)

{-
mysteryfun1 takes a function f as argument, and f :: a -> b -> c
and returns a function which has type b -> a -> c

for eaxmple,
mysteryfun1 f b a = f a b. has this type

And mysteryfun2 takes a function f :: a -> a -> b.
And returns a function which has the same type as f.
For example, the mysteryfun1 f = f has this type

And we notice that, in mysteryfun2, the first and the second argument of f has to have the same type,
which in mysteryfun1 is not.

And in all the possible behavior of mysteryfun2 is also possible for mysteryfun1.

Since if we let b == a, then mysteryfun1 :: (a -> b -> c) -> (b -> a -> c) has the type mysteryfun2 :: (a -> a -> b) -> (a -> a -> b)

-}

-- Exercise 3

-- Q3.1

newtype Down a = Down a   deriving (Show,Eq)

instance Ord a => Ord (Down a) where
   compare (Down x) (Down y) = case compare x y of
                                    EQ -> EQ
                                    LT -> GT 
                                    GT -> LT
-- ...

-- Q3.2

data BT a = Empty | Fork a (BT a) (BT a)  deriving (Show)

instance Eq a => Eq (BT a) where
   Empty == Empty                          = True
   Empty == Fork _ _ _                     = False
   Fork _ _ _ == Empty                     = False
   Fork a (a') (a'') == Fork b (b') (b'')  = case a == b of
                                                True -> (a' == b') && (a'' == b'')
                                                False -> False

instance Ord a => Ord (BT a) where
   compare Empty Empty                             = EQ
   compare Empty (Fork _ _ _)                      = LT
   compare (Fork _ _ _) Empty                      = GT
   compare (Fork a (a') (a'')) (Fork b (b') (b'')) = case compare a b of
                                                   LT -> LT
                                                   GT -> GT
                                                   EQ -> case compare a' b' of
                                                            LT -> LT
                                                            GT -> GT
                                                            EQ -> compare a'' b''

-- Q3.3

class Mon m where
   one :: m
   (<.>) :: m -> m -> m
   
   mul :: Foldable t => t m -> m
   mul = foldr (<.>) one
   
   --unit_one a = one <.> a == a && a <.> one == a
   --assoc a b c = (a <.> b) <.> c == a <.> (b <.> c)

instance Mon [s] where
   one = []
   (<.>) = (++)

instance Mon m => Mon (a -> m) where
   one = \x -> one
   (<.>) = \f g -> (\x -> (f x) <.> (g x))

instance Mon m => Mon (Down m) where
   one = Down one
   (<.>) = \(Down x) (Down y) -> Down (y <.> x)
-- ...

-- Exercise 4

-- If you've made it through the other exercises and the suggested
-- reading, you can have a go at implementing randomized quicksort below!
-- You only need to complete the rqsort function.
  
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (p:xs) = qsort [x | x <- xs, x < p]
                ++ [p]
                ++ qsort [x | x <- xs, x >= p]  

{-
data Rand a = Generator (StdGen -> (a , StdGen))

instance Monad Rand where
  return x = Generator (\g -> (x,g))
  Generator h >>= f = Generator (\g -> let (x, g') = h g
                                           (Generator h') = f x
                                       in h' g')
                     
instance Functor Rand where
   fmap f xm = xm >>= return . f

instance Applicative Rand where
   pure = return
   fm <*> xm = fm >>= \f -> xm >>= return . f

runRand :: Int -> Rand a -> a
runRand seed (Generator h) = fst (h (mkStdGen seed))

randInt :: Rand Int
randInt = Generator random

randIntR :: (Int, Int) -> Rand Int
randIntR (lower, upper) = Generator (randomR (lower, upper))

uniform :: [a] -> Rand a
uniform [] = undefined
uniform xs = do
              n <- randIntR (0, length xs - 1)
              return(xs !! n)

getPivot :: [a] -> Int -> (a, [a])
getPivot (x:xs) 0 = (x,xs)
getPivot (x:xs) n = let (p,ys) = getPivot xs (n-1) in (p, x:ys)

rqsort :: Ord a => [a] -> Rand [a]
rqsort xs = undefined

rqsort' :: Ord a => [a] -> [a]
rqsort' xs = runRand seed (rqsort xs)
             where seed = 42 -- say
-}