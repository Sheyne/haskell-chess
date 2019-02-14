module Lib
    ( whiteBackRow
    , blackBackRow
    , initialBoard
    , showBoard
    , isValidMove
    , putAt
    , unfilledBoard
    , pieceAt
    , Piece(..)
    , Color(..)
    , Square(..)
    ) where

import Data.Sequence
import Data.List (intercalate)
import Data.Foldable (toList)

data Piece = King | Queen | Bishop | Knight | Rook | Pawn deriving Show
data Color = White | Black deriving Show
data Square = Filled {color :: Color, piece :: Piece} | Unfilled

instance Show Square where
    show Unfilled              = "_"
    show (Filled White King)   = "♔"
    show (Filled White Queen)  = "♕"
    show (Filled White Rook)   = "♖"
    show (Filled White Bishop) = "♗"
    show (Filled White Knight) = "♘"
    show (Filled White Pawn)   = "♙"
    show (Filled Black King)   = "♚"
    show (Filled Black Queen)  = "♛"
    show (Filled Black Rook)   = "♜"
    show (Filled Black Bishop) = "♝"
    show (Filled Black Knight) = "♞"
    show (Filled Black Pawn)   = "♟"

type Board = Seq (Seq Square)

whiteBackRow :: Seq Square
whiteBackRow = fromList [Filled White Rook
                        ,Filled White Knight
                        ,Filled White Bishop
                        ,Filled White King
                        ,Filled White Queen
                        ,Filled White Bishop
                        ,Filled White Knight
                        ,Filled White Rook]

blackBackRow :: Seq Square
blackBackRow = fmap (\x -> x {color = Black}) whiteBackRow

initialBoard :: Board
initialBoard = fromList ([ blackBackRow,
                           Data.Sequence.replicate 8 (Filled Black Pawn)] ++ Prelude.replicate 4 (Data.Sequence.replicate 8 Unfilled) ++ [
                           Data.Sequence.replicate 8 (Filled White Pawn),
                           whiteBackRow])

unfilledBoard :: Board
unfilledBoard = Data.Sequence.replicate 8 (Data.Sequence.replicate 8 Unfilled)

putAt :: (Int, Int) -> Square -> Board -> Board
putAt (r, c) v b = update r (update c v (b `index` r)) b

showRow :: Seq Square -> String
showRow b = unwords (toList (fmap show b))

showBoard :: Board -> String
showBoard b = intercalate "\n" (map showRow (toList b))

pieceAt :: Board -> (Int, Int) -> Square
pieceAt b (r, c) = b `index` r `index` c

isValidMove2 :: Board -> (Int, Int) -> (Int, Int) -> Square -> Square -> Bool
isValidMove2 _ _ _ Unfilled _ = False
isValidMove2 _ _ _ (Filled White _) (Filled White _) = False
isValidMove2 _ _ _ (Filled Black _) (Filled Black _) = False
isValidMove2 _ (r1, c1) (r2, c2) (Filled _ Knight) _ = let rd = abs (r1 - r2)
                                                           cd = abs (c1 - c2)
                                                        in (rd, cd) == (1, 2) || (rd, cd) == (2, 1)

isValidMove :: Board -> (Int, Int) -> (Int, Int) -> Bool
isValidMove b l1 l2 = isValidMove2 b l1 l2 (b `pieceAt` l1) (b `pieceAt` l2)


tf (r1, c1) (r2, c2) = let rd = abs r1 - r2 ; cd = abs c1 - c2 ; in (rd, cd) == (1, 2) || (rd, cd) == (2, 1)
