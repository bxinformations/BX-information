-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Assessed2 where

import Data.List
import Data.Tree

import System.Random

import Types
import DomViz
{- 
  Uncomment the previous line if you want to use the visualization
  routines. You will need to have the diagrams library, which you
  should be able to install by running:

  $ cabal install --lib diagrams diagrams-contrib diagrams-lib diagrams-svg

-}

-- given a cell c and a player p, compute the adjacent cell c'
-- that is also occupied if p plays a domino at c
adjCell :: Cell -> Player -> Cell
adjCell (x,y) H = (x+1,y)
adjCell (x,y) V = (x,y+1)

-- compute the opponent of a player
opp :: Player -> Player
opp H = V
opp V = H

-- determine whether a move is valid in a given board
valid :: Board -> Cell -> Bool
valid b c = c `elem` free b && adjCell c (turn b) `elem` free b

-- create an empty board from an arbitrary list of cells
empty :: [Cell] -> Board
empty cs = Board { turn = H, free = cs, hist = [] }

-- create a rectangular board of arbitrary dimensions
board :: Int -> Int -> Board
board maxx maxy = empty [(x,y) | x <- [1..maxx], y <- [1..maxy]]

-- create a crosshatch-shaped square board of arbitrary dimension
hatch :: Int -> Board
hatch n = empty [(x,y) | x <- [1..2*n+1], y <- [1..2*n+1], odd y || x == 1 || x == (2*n+1) || odd x]

-- some example Domineering games
board4x4_3 = Board { turn = H,
                     free = [(1,1),(1,2),(2,2),(2,3),(2,4),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)],
                     hist = [(1,3),(2,1)] }

alphaDom_vs_LeeSedom =
  Board { turn = V,
          free = [(-4,1),(-4,3),(-2,0),(-2,4),(2,1),(2,4),(3,-4),(3,4),(4,-2),(4,0)],
          hist = [(0,4),(4,1),(0,-4),(-4,-3),(-1,-2),(2,-1),(-2,-4),(-4,-1),(-1,2),(4,3),(1,2),(-2,2),(-4,-4),(-2,-2),(2,-2),(4,-4),(-3,1),(2,-4),(-4,4),(-1,3),(-4,2),(-3,-2),(3,-1),(1,-3),(-2,-3),(3,1),(1,3)] }

alphaDom_vs_RanDom =
  Board { turn = V,
          free = [(-4,-3),(-4,0),(-2,-4),(-2,-2),(-1,-4),(-1,-2),(-1,2),(-1,4),(0,-4),(0,-2),(0,2),(0,4),(1,-4),(1,-2),(1,2),(1,4),(2,-4),(2,-2),(2,4),(3,-4),(4,0),(4,3)],
          hist = [(-3,4),(2,-1),(-3,2),(4,-2),(-4,-4),(-4,3),(3,4),(2,1),(-3,1),(3,1),(-4,-1),(-2,-1),(-2,3),(-4,1),(1,3),(4,-4),(-4,-2),(4,1),(1,-3),(3,-2),(-2,-3)] }

-- start of Exercise 1

-- Q1.1
validPlayer :: Board -> Player -> Cell -> Bool
validPlayer b p c = c `elem` free b && adjCell c p `elem` free b

legalMoves :: Player -> Board -> [Cell]
legalMoves p board = [c | c <- free board, validPlayer board p c]

-- Q1.2
moveLegal :: Board -> Cell -> Board
moveLegal b c = case valid b c of
                  True -> Board {turn = opp (turn b),
                                 free = (free b) \\ [c, adjCell c (turn b)],
                                 hist = [c] ++ (hist b)}
                  False -> error("illegal move")

-- Q1.3
replay :: Board -> [Board]
replay b = case null (hist b) of
              True -> [b]
              False -> do let now = Board {turn = opp (turn b),
                                          free = (free b) ++ [head (hist b), 
                                                  adjCell (head (hist b)) (opp (turn b))],
                                          hist = tail (hist b)}
                          replay now ++ [b]

-- start of Exercise 2

gametree :: Board -> Tree Board
gametree b = Node b [gametree (moveLegal b c) | c <- legalMoves (turn b) b]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

-- Q2.1
sign :: Player -> Int
sign H = -1
sign V = 1

score :: Board -> Score
score b = case null (legalMoves (turn b) b) of
            True -> Win (opp (turn b))
            False -> Heu ((length (legalMoves V b)) - (length (legalMoves H b)) - (sign (turn b)))

-- Q2.2
minimax :: (Board -> Score) -> Tree Board -> Tree (Board, Score)
minimax f (Node g []) = Node (g, f g) []
minimax f (Node g ts) 
   | (turn g) == H = Node (g, minimum ps) ts'
   | otherwise = Node (g, maximum ps) ts'
                   where
                      ts' = map (minimax f) ts
                      ps  = [p | Node (_, p) _ <- ts']

-- Q2.3
bestmoves :: Int -> (Board -> Score) -> Board -> [Cell]
bestmoves d f b = [head (hist g') | Node (g',p') _ <- ts, p' == best]
                  where 
                    tree = prune d (gametree b)
                    Node (g, best) ts = minimax f tree


-- start of Exercise 3

class Monad m => SelectMonad m where
  select :: [a] -> m a

instance SelectMonad [ ] where
  select xs = xs

instance SelectMonad IO where
  select xs
    | not (null xs) = do i <- getStdRandom (randomR (0, length xs-1))
                         return (xs !! i)
    | otherwise     = error ("cannot select from empty list")


selectSafe :: SelectMonad m => [a] -> m (Maybe a)
selectSafe [] = return Nothing
selectSafe xs = select xs >>= \x -> return (Just x)

randomBestPlay :: SelectMonad m => Int -> (Board -> Score) -> Board -> m (Maybe Cell)
randomBestPlay d sfn = selectSafe . bestmoves d sfn

randomPlay :: SelectMonad m => Board -> m (Maybe Cell)
randomPlay b = selectSafe (legalMoves (turn b) b)

-- Q3.1
runGame :: SelectMonad m => (Board -> m (Maybe Cell)) -> (Board -> m (Maybe Cell)) -> Board -> m Board
runGame h v b
  | (turn b == H) = do t <- h b
                       case t of
                          Nothing -> return b
                          Just c -> return (moveLegal b c)
  | otherwise = do t <- v b
                   case t of
                          Nothing -> return b
                          Just c -> return (moveLegal b c)

-- Q3.2
inCarpet :: Int -> Int -> Bool
inCarpet 0 _ = True
inCarpet _ 0 = True
inCarpet x y = not ((xr == 1) && (yr == 1)) && inCarpet xq yq
  where ((xq, xr), (yq, yr)) = (x `divMod` 3, y `divMod` 3)

carpets :: [Board]
carpets = [empty [(x + 1 , y + 1) | x <- [0..(3^n-1)], y <- [0..(3^n-1)], inCarpet x y] | n <- [0..]]

