{-|
Module      : Cells
Description : A sequence of Cells from a Sudoku
Copyright   : (c) Frédéric BISSON 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This module handles `Seq` of `Cell` from a Sudoku grid.

-}
module Sudoku.Cells (Cells, subseq, tokens, validCells) where 

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes)

import Data.Foldable (toList)

import Sudoku.Cell (Cell (Cell, cValue, cPossibles))
import Sudoku.Token (Token)

{-|
A `Cells` is just an alias for `Seq` of `Cell`.
-}
type Cells = Seq Cell

{-|
Returns a subsequence from a `Seq`.
-}
subseq :: Int -- ^ Start index (from 0)
       -> Int -- ^ Length
       -> Seq a -- ^ The source `Seq`
       -> Seq a -- ^ The resulting `Seq`
subseq start len = Seq.take len . Seq.drop start

{-|
Given a `Cells`, returns a `Set` of all the `Token`.
-}
tokens :: Cells -> Set Token
tokens = foldr addCell Set.empty
    where addCell :: Cell -> Set Token -> Set Token
          addCell (Cell (Just t) _) s = Set.insert t s
          addCell _ s = s

{-|
Tells if a `Seq` of `Cell` is valid for a Sudoku grid. Such a `Cells` must have
the following properties:

- no duplicate `Token`
- at least one `Token` in the `cPossibles` field of each `Cell`

-}
validCells :: Cells -> Bool
validCells cs = length ts == Set.size (Set.fromList ts)
              && notElem Set.empty (cPossibles <$> cs)
    where ts = catMaybes $ toList $ cValue <$> cs
