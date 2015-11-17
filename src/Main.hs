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

import Sudoku.Sudoku (prettyPrint)
import Sudoku.Reader (toSudoku)
import Sudoku.Solver (repeatBruteReduces)

{-|
Process Sudokus from `[String]`. It prints the problem and then its solution,
and continues with the next problem.
-}
processSudokus :: [String] -> IO ()
processSudokus [] = putStrLn "The end!"
processSudokus grids = do
    let (sudoku, remaining) = toSudoku grids

    putStrLn $ prettyPrint sudoku

    let solutions = repeatBruteReduces sudoku
    
    case solutions of
         (solution:_) -> putStrLn $ prettyPrint solution
         _ -> putStrLn "No solution!"

    processSudokus remaining

main :: IO ()
main = do
    [filename] <- getArgs

    grids <- liftM lines (readFile filename)

    processSudokus grids
