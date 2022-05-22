import CNF
import CNF.DIMACS
import Control.Monad
import qualified Solver.Backtracking as Backtracking
import qualified Solver.Naive as Naive
import System.Environment
import System.Exit
import qualified Tests.Tester as Tester

main :: IO ()
main = do
  name <- getProgName
  args <- getArgs
  unless (length args == 1) $ do
    putStrLn ("Usage: " ++ name ++ " <cnf file>")
    exitFailure
  f <- readCNFfromDIMACS (head args)
  case Backtracking.solution f of
    Nothing -> putStrLn "UNSAT"
    Just rho -> putStrLn ("SAT\n" ++ dimacsSubst rho ++ "\nValide : " ++ show (Tester.test rho f))
