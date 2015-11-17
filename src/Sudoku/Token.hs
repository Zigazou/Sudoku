{-|
Module      : Token
Description : Token populates Cell of a Sudoku grid
Copyright   : (c) Frédéric BISSON 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This module handles Token.
-}
module Sudoku.Token
( Token (T1, T2, T3, T4, T5, T6, T7, T8, T9)
, readToken
)
where

{-|
A Sudoku can have 9 Token.
-}
data Token = T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9
             deriving (Eq, Ord, Enum, Read)

instance Show Token where
    show t = show (1 + fromEnum t)

{-|
Given a `Char`, returns the corresponding `Token`, or `Nothing` for anything
else.
-}
readToken :: Char -> Maybe Token
readToken '1' = Just T1
readToken '2' = Just T2
readToken '3' = Just T3
readToken '4' = Just T4
readToken '5' = Just T5
readToken '6' = Just T6
readToken '7' = Just T7
readToken '8' = Just T8
readToken '9' = Just T9
readToken _ = Nothing
