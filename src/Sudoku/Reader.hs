{-|
Module      : Reader
Description : Simple parser of Sudoku
Copyright   : (c) Frédéric BISSON 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This module implements a very crude parser of a Sudoku grid.
-}
module Sudoku.Reader (toSudoku) where

import qualified Data.Sequence as Seq
import Control.Monad (msum)

import Sudoku.Sudoku (Sudoku (Sudoku))
import Sudoku.Cell (makeCell, isSet)
import Sudoku.Cells (Cells)
import Sudoku.Token (readToken)

{-|
Read a `String` and generate a `Cells`. The `String` must have the following
properties:

- starts with a '|',
- have 9 digits (from 1 to 9) or space to describe a cell,
- the cells are separated with one space,
- ends with a '|'.

Example:

@
    |  2   3 7 8     6|
@

Every `String` with additional or less characters will be rejected.
-}
readRow :: String -> Cells
readRow [ '|'
        , a, ' ', b, ' ', c, ' '
        , d, ' ', e, ' ', f, ' '
        , g, ' ', h, ' ', i
        , '|'
        ] = Seq.fromList $ (makeCell . readToken) <$> [a,b,c,d,e,f,g,h,i]
readRow s = error $ "Incorrect line: '" ++ s ++ "'"

{-|
Read a `[String]` and generate a `Sudoku` grid. The `[String]` must have the
following properties:

- starts with "+-----------------+",
- have 9 `String` which can be parsed by `readRow`
- ends with "+-----------------+".

Example:

@
    +-----------------+
    |7 4   5          |
    |  2   3 7 8     6|
    |          9   5 7|
    |4     7          |
    |9               3|
    |          3     2|
    |1 5   9          |
    |2     1 4 7   9  |
    |          2   1 4|
    +-----------------+
@

Every `[String]` with additional or less characters will be rejected.
-}
toSudoku :: [String] -> (Sudoku, [String])
toSudoku ("+-----------------+"
         :a:b:c:d:e:f:g:h:i
         :"+-----------------+"
         :remaining
         ) = (Sudoku rows filled, remaining)
    where rows = Seq.fromList $ readRow <$> [a,b,c,d,e,f,g,h,i]
          filled = Seq.length $ Seq.filter isSet (msum rows)
toSudoku s = error $ "Incorrect Grid: '" ++ show s ++ "'"
