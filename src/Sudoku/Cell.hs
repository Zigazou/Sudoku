{-|
Module      : Cell
Description : A Cell of a Sudoku
Copyright   : (c) Frédéric BISSON 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This module handles a `Cell` in a Sudoku grid. It keeps track of every possible
`Token` the `Cell` could have.

-}
module Sudoku.Cell
( Cell (Cell, cValue, cPossibles)
, makeCell
, removePossibles
, isSet
)
where 

import Data.Set (Set)
import qualified Data.Set as Set

import Sudoku.Token (Token (T1, T9))

{-|
A `Cell` may contain a `Token` or be empty.
-}
data Cell = Cell
    { cValue :: Maybe Token -- ^ The `Token` or `Nothing`
    , cPossibles :: Set Token -- ^ Possible `Token`s the `Cell` could contain
    }

instance Eq Cell where
    (==) c1 c2 = cValue c1 == cValue c2

instance Show Cell where
    show (Cell Nothing _) = " "
    show (Cell (Just t) _) = show t

{-|
Make a `Cell` given a `Token`. It initializes the `cPossibles` field with
every `Token` possible if the `Cell` is empty. If the `Cell` is not empty, the
`cPossibles` field is initialized with the `Token`.
-}
makeCell :: Maybe Token -> Cell
makeCell mt@(Just t) = Cell mt (Set.singleton t)
makeCell _           = Cell Nothing (Set.fromList [T1 .. T9])

{-|
Given a `Cell`, it removes all the `Token`s from the given `Set` from this
`Cell` possible tokens. If there remains one and only one possible `Token`,
the `Cell` is automatically filled with this `Token`.
-}
removePossibles :: Cell -> Set Token -> Cell
removePossibles (Cell v ps) ts = Cell value possibles
    where possibles = Set.difference ps ts
          value | Set.size possibles == 1 = Just $ Set.elemAt 0 possibles
                | otherwise = v

{-|
Tells if a `Cell` is filled with a `Token` or not.
-}
isSet :: Cell -> Bool
isSet (Cell (Just _) _) = True
isSet _ = False
