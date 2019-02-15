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

isUnfilled :: Square -> Bool
isUnfilled Unfilled = True
isUnfilled _ = False

isValidMove3 :: Board -> (Int, Int) -> (Int, Int) -> (Int, Int, Int, Int) -> Square -> Square -> Bool
isValidMove3 _ _ _ (d1, d2, _, _) (Filled _ Knight) _ = (d1, d2) == (1, 2) || (d1, d2) == (2, 1)
isValidMove3 _ _ _ (rd, cd, _, _) (Filled _ King) _ = rd <= 1 && cd <= 1
isValidMove3 _ _ _ (1, 0, 1, 0) (Filled White Pawn) Unfilled = True
isValidMove3 _ _ _ (1, 0, -1, 0) (Filled Black Pawn) Unfilled = True
isValidMove3 _ _ _ (1, 1, 1, _) (Filled White Pawn) (Filled _ _) = True
isValidMove3 _ _ _ (1, 1, -1, _) (Filled Black Pawn) (Filled _ _) = True
isValidMove3 b (r1, c1) ts@(r2, c2) d@(rd, cd, rsgn, csgn) p@(Filled _ piece) finalLoc = (case piece of Rook -> rd == 0 || cd == 0
                                                                                                        Bishop -> rd == cd
                                                                                                        Queen -> rd == cd || rd == 0 || cd == 0
                                                                                                        _ -> False)
                                                                                            && (newLoc == ts || (isUnfilled newSq
                                                                                            && isValidMove3 b newLoc ts d p finalLoc))

                                                                            where rs = rsgn + r1
                                                                                  cs = csgn + c1
                                                                                  newLoc = (rs, cs)
                                                                                  newSq = pieceAt b newLoc
isValidMove3 _ _ _ _ _ _ = False


isValidMove2 :: Board -> (Int, Int) -> (Int, Int) -> Square -> Square -> Bool
isValidMove2 _ l1 l2 _ _
                       | l1 == l2 = True
isValidMove2 _ _ _ Unfilled _ = False
isValidMove2 _ _ _ (Filled White _) (Filled White _) = False
isValidMove2 _ _ _ (Filled Black _) (Filled Black _) = False
isValidMove2 b l1@(r1, c1) l2@(r2, c2) s1 s2 = isValidMove3 b l1 l2 (abs (r1 - r2), abs (c1 - c2), signum (r2 - r1), signum (c2 - c1)) s1 s2


isValidMove :: Board -> (Int, Int) -> (Int, Int) -> Bool
isValidMove b l1 l2 = isValidMove2 b l1 l2 (b `pieceAt` l1) (b `pieceAt` l2)