{-|
Module      : Main
Description : Interface for the Sudoku solver
Copyright   : (c) Frédéric BISSON 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Interface for the Sudoku solver.
-}
module Main where

import System.Environment (getArgs)
import Control.Monad (liftM)
import Control.Parallel.Strategies (parMap, rpar)

import Sudoku.Sudoku (Sudoku, prettyPrint)
import Sudoku.Reader (toSudoku)
import Sudoku.Solver (repeatBruteReduces)

{-|
Print results.
-}
printResults :: [(Sudoku, Either String Sudoku)] -> IO ()
printResults results = mapM_ printResult results
    where printResult (problem, eSolution) = do
            putStrLn $ prettyPrint problem
            putStrLn $ case eSolution of
                            Left message -> message
                            Right solution -> prettyPrint solution

{-|
Parse multiple grids and returns the corresponding `Sudoku`s.
-}
readSudokus :: [String] -> [Sudoku]
readSudokus [] = []
readSudokus grids = sudoku:readSudokus remaining
    where (sudoku, remaining) = toSudoku grids

{-|
Find a solution for one `Sudoku`.
-}
findSolution :: Sudoku -> Either String Sudoku
findSolution sudoku = case repeatBruteReduces sudoku of
                           (solution:_) -> Right solution
                           _ -> Left "No solution!"

{-|
Find each solution for a `List` of `Sudoku`.
-}
findSolutions :: [Sudoku] -> [Either String Sudoku]
findSolutions = parMap rpar findSolution

main :: IO ()
main = do
    [filename] <- getArgs

    grids <- liftM lines (readFile filename)

    let sudokus = readSudokus grids
        solutions = findSolutions sudokus
        results = zip sudokus solutions

    printResults results
