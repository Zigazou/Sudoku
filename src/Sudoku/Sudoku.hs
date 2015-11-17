{-|
Module      : Sudoku
Description : Functions for handling a Sudoku grid
Copyright   : (c) Frédéric BISSON 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This module gives functions for handling a Sudoku grid.
-}
module Sudoku.Sudoku
( Sudoku (Sudoku, sRows, sFilled)
, Coords
, Cell
, prettyPrint
, setCell
, getCell
, getRow
, getColumn
, getSquare
, isComplete
, allCoords
, validSudoku
, firstEmpty
) where

import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Control.Monad (msum)

import Sudoku.Cell (Cell, isSet)
import Sudoku.Cells (Cells, subseq, validCells)

{-|
Coordinates are simply a couple of `Int`
-}
type Coords = (Int, Int)

{-|
A `Sudoku` is composed of `sRows` (each containing a `Seq` of `Cell`) and
an `sFilled` field which optimizes the `isComplete` function.
-}
data Sudoku = Sudoku
    { sRows :: Seq Cells -- ^ All the `Cell` of the `Sudoku`
    , sFilled :: Int -- ^ Number of filled `Cells`
    } deriving (Eq)

printRow :: Cells -> String
printRow cs = "|" ++ (intersperse ' ' . concat . toList . fmap show) cs ++ "|"

{-|
Pretty print a `Sudoku` grid.
-}
prettyPrint :: Sudoku -> String
prettyPrint s = unlines $ 
    [ "+-----------------+" ]
    ++ toList (printRow <$> sRows s)
    ++ [ "+-----------------+" ]

{-|
Returns `Cells` from a specific row in a `Sudoku` grid.
-}
getRow :: Sudoku -> Coords -> Cells
getRow s (_, y) = Seq.index (sRows s) y

{-|
Returns `Cells` from a specific column in a `Sudoku` grid.
-}
getColumn :: Sudoku -> Coords -> Cells
getColumn s (x, _) = Seq.fromList [ Seq.index row x | row <- toList $ sRows s ]

{-|
Returns `Cells` from a specific square in a `Sudoku` grid.
-}
getSquare :: Sudoku -> Coords -> Cells
getSquare s (x, y) = msum $ subseq sx 3 <$> subseq sy 3 (sRows s)
    where sx = 3 * div x 3
          sy = 3 * div y 3

{-|
Returns a `List` of the `Cells` of each row of the `Sudoku` grid.
-}
getRows :: Sudoku -> [Cells]
getRows = toList . sRows

{-|
Returns a `List` of the `Cells` of each column of the `Sudoku` grid.
-}
getColumns :: Sudoku -> [Cells]
getColumns s = getColumn s <$> [(x, 0) | x <- [0..8] ]

{-|
Returns a `List` of the `Cells` of each square of the `Sudoku` grid.
-}
getSquares :: Sudoku -> [Cells]
getSquares s = [ msum $ subseq x 3 <$> subseq y 3 (sRows s)
               | y <- [0, 3, 6], x <- [0, 3, 6]
               ]

{-|
Returns a `Cell` at given `Coords` in a `Sudoku` grid.
-}
getCell :: Sudoku -> Coords -> Cell
getCell s (x, y) = Seq.index (Seq.index (sRows s) y) x

{-|
Sets the `Cell` at given `Coords` in a `Sudoku` grid. It automatically updates
the `sFilled` field.
-}
setCell :: Sudoku -> Coords -> Cell -> Sudoku
setCell s (x, y) cell = Sudoku rows filled
    where rows = Seq.adjust (Seq.update x cell) y (sRows s)
          filled = sFilled s + if isSet cell then 1 else 0

{-|
Tells if a `Sudoku` grid has been completed. It should contain exactly 81
filled `Cell`.
-}
isComplete :: Sudoku -> Bool
isComplete = (==) 81 . sFilled

{-|
Returns all the possible `Coords` for a `Sudoku` grid.
-}
allCoords :: [Coords]
allCoords = [ (x, y) | y <- [0..8], x <- [0..8] ]

{-|
Tells if a `Sudoku` grid is valid. To be valid, it must have the following
properties:

- all rows must be valid
- all columns must be valid
- all subsquares must be valid
-}
validSudoku :: Sudoku -> Bool
validSudoku s = all validCells (getRows s)
             && all validCells (getColumns s)
             && all validCells (getSquares s)

{-|
Returns the first empty `Cell` `Coords`, if it exists.
-}
firstEmpty :: Sudoku -> Maybe Coords
firstEmpty s | isComplete s = Nothing
             | otherwise = Just $ head empties
    where empties = [ pos | pos <- allCoords, not $ isSet $ getCell s pos ]
