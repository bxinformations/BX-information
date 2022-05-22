module CNF.DIMACS (readCNFfromDIMACS, writeCNFtoDIMACS, dimacsSubst) where

import CNF

import Data.List
import System.IO

import Text.ParserCombinators.Parsec

-- parse a positive natural number
pos :: Parser Int
pos = do
  x <- satisfy (\c -> c >= '1' && c <= '9')
  xs <- many digit
  return (read (x:xs))

-- parse a non-zero integer
int :: Parser Int
int = do char '-'
         n <- pos
         return (-n)
     <|> pos

-- parse a literal with variables in range 1..n
lit :: Int -> Parser Lit
lit n = do
  spaces
  k <- int
  spaces
  let v = abs k
  if v > n then unexpected ("variable " ++ show v ++ " out of bounds (" ++ show n ++ ")")
  else return (Lit v (v == k))

-- parse a clause with variables in range 1..n
cls :: Int -> Parser Cls
cls n = do
  ls <- manyTill (lit n) (char '0')
  spaces
  return (BigOr ls)

-- parse a comment
comment :: Parser String
comment = char 'c' >> manyTill anyChar newline

-- parse a CNF in DIMACS format
dimacs :: Parser CNF
dimacs = do
  many comment
  string "p cnf"
  space
  n <- pos
  space
  m <- pos
  spaces
  cs <- manyTill (cls n) eof
  if length cs /= m then
    unexpected (show (length cs) ++ " clauses found but expected " ++ show m)
  else return (BigAnd [1..n] cs)

readCNFfromDIMACS :: FilePath -> IO CNF
readCNFfromDIMACS file = do
  mf <- parseFromFile dimacs file
  case mf of
    Right frm -> return frm
    Left err -> do
      print err
      error ("readDIMACS: parse error")

dimacsLit :: Lit -> String
dimacsLit (Lit v b) = (if b then "" else "-") ++ show v

dimacsCls :: Cls -> String
dimacsCls c = foldr (\l s -> dimacsLit l ++ " " ++ s) "0" (literals c)

dimacsCNF :: CNF -> String
dimacsCNF f =
  "p cnf " ++ show n ++ " " ++ show k ++ "\n" ++
  concatMap (\c -> dimacsCls c ++ "\n") (clauses f)
  where
    n = length (vars f)
    k = length (clauses f)

writeCNFtoDIMACS :: FilePath -> CNF -> IO ()
writeCNFtoDIMACS file f = do
  h <- openFile file WriteMode
  hPutStr h (dimacsCNF f)
  hClose h
  return ()

dimacsSubst :: [(Var,Bool)] -> String
dimacsSubst rho = intercalate " " [dimacsLit (Lit v b) | (v,b) <- rho]
