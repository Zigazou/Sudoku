{-|
Module      : Solver
Description : Sudoku solver routines
Copyright   : (c) Frédéric BISSON 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This module gives functions for solving a Sudoku grid.
-}
module Sudoku.Solver (repeatEasyReduces, repeatBruteReduces) where

import Data.Maybe (fromJust, isJust)
import Data.List (find, nub)
import Data.Foldable (toList)

import Sudoku.Sudoku ( Sudoku, setCell, getRow, getColumn, allCoords, getCell
                     , getSquare, Coords, isComplete, validSudoku, firstEmpty
                     )
import Sudoku.Cell (isSet, removePossibles, Cell (cPossibles), makeCell)
import Sudoku.Cells (tokens)

easyReduce :: Sudoku -> Coords -> Sudoku
easyReduce s pos
    | isSet current = s
    | otherwise = setCell s pos (removePossibles current (mconcat ts))
    where current = getCell s pos
          ts = tokens <$> [getColumn s pos, getRow s pos, getSquare s pos]

easyReduces :: Sudoku -> Sudoku
easyReduces sudoku = foldl easyReduce sudoku allCoords

{-|
Finds `Cell` which can have only one possible `Token` and fills them until it
can not be done anymore.
-}
repeatEasyReduces :: Sudoku -> Sudoku
repeatEasyReduces s | s == ns = s
                    | otherwise = repeatEasyReduces ns
                    where ns = easyReduces s

bruteReduce :: Sudoku -> Coords -> [Sudoku]
bruteReduce s pos
    | isSet current = []
    | otherwise = toList $ setCell s pos <$> cells 
    where current = getCell s pos
          cells = (makeCell . Just) <$> toList (cPossibles current)

bruteReduces :: Sudoku -> [Sudoku]
bruteReduces sudoku = case firstEmpty sudoku of
                           Nothing -> [sudoku]
                           Just pos -> bruteReduce sudoku pos

{-|
Nearly brute force algorithm to find solution for a `Sudoku`. It’s nearly
brute force because it uses the `repeatEasyReduces` to reduce the tree.
-}
repeatBruteReduces :: Sudoku -> [Sudoku]
repeatBruteReduces s | isComplete s' = [s']
                     | null ns = []
                     | isJust complete = [fromJust complete]
                     | otherwise = nub $ concatMap repeatBruteReduces ns
    where s' = repeatEasyReduces s
          ns = filter validSudoku (bruteReduces s')
          complete = find isComplete ns
