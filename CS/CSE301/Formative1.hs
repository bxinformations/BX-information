import Distribution.Parsec.Newtypes (List)
import System.IO (isEOF)
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y
                      then x : merge xs (y:ys)
                      else y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort l) (msort r)
               where
                   k = length xs `div` 2
                   l = take k xs
                   r = drop k xs

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x1:x2:xs) = (x1 <= x2) && isSorted (x2:xs)

getList :: [String] -> IO [String]
getList list =  do end <- isEOF
                   if end
                      then return list
                      else do
                          line <- getLine
                          getList (line : list)

main :: IO()
main = do
    end <- isEOF
    if end 
    then return()
    else do
        l <- getList []
        putStrLn (unlines (msort l))