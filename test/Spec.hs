import Test.Hspec
import Lib

main :: IO ()
main = hspec $
    describe "Chess" $ do
    it "knight moves in L" $ do
        isValidMove (putAt (2, 2) (Filled White Knight) unfilledBoard) (2, 2) (3, 4) `shouldBe` True
        isValidMove (putAt (2, 2) (Filled White Knight) unfilledBoard) (2, 2) (3, 3) `shouldBe` False
    it "can't move on self" $
        let board = putAt (2, 2) (Filled White Knight) $ putAt (3, 4) (Filled White Knight) unfilledBoard
            in isValidMove board (2, 2) (3, 4) `shouldBe` False
    it "lets knights capture" $
        isValidMove (putAt (3, 4) (Filled Black Knight) (putAt (2, 2) (Filled White Knight) unfilledBoard)) (2, 2) (3, 4) `shouldBe` True
    it "only lets bishops move in diagonals" $ do
        isValidMove (putAt (2, 2) (Filled White Bishop) unfilledBoard) (2, 2) (4, 4) `shouldBe` True
        isValidMove (putAt (2, 2) (Filled White Bishop) unfilledBoard) (2, 2) (3, 4) `shouldBe` False
        isValidMove (putAt (2, 2) (Filled White Bishop) unfilledBoard) (2, 2) (0, 4) `shouldBe` True
        isValidMove (putAt (2, 2) (Filled White Bishop) unfilledBoard) (2, 2) (1, 3) `shouldBe` True
        isValidMove (putAt (2, 2) (Filled White Bishop) unfilledBoard) (2, 2) (1, 3) `shouldBe` True
        isValidMove (putAt (2, 2) (Filled White Bishop) unfilledBoard) (2, 2) (2, 3) `shouldBe` False
    it "only lets rooks move straight" $ do
        isValidMove (putAt (2, 2) (Filled White Rook) unfilledBoard) (2, 2) (2, 5) `shouldBe` True
        isValidMove (putAt (2, 2) (Filled White Rook) unfilledBoard) (2, 2) (4, 2) `shouldBe` True
        isValidMove (putAt (2, 2) (Filled White Rook) unfilledBoard) (2, 2) (0, 2) `shouldBe` True
        isValidMove (putAt (2, 2) (Filled White Rook) unfilledBoard) (2, 2) (2, 1) `shouldBe` True
        isValidMove (putAt (2, 2) (Filled White Rook) unfilledBoard) (2, 2) (1, 1) `shouldBe` False
        isValidMove (putAt (2, 2) (Filled White Rook) unfilledBoard) (2, 2) (5, 1) `shouldBe` False
        isValidMove (putAt (2, 2) (Filled White Rook) unfilledBoard) (2, 2) (3, 1) `shouldBe` False
    it "doesn't let rooks move through pieces" $
        isValidMove (putAt (2, 4) (Filled Black Knight) (putAt (2, 2) (Filled White Rook) unfilledBoard)) (2, 2) (2, 5) `shouldBe` False
    it "allows rook captures" $
        isValidMove (putAt (2, 4) (Filled Black Knight) (putAt (2, 2) (Filled White Rook) unfilledBoard)) (2, 2) (2, 4) `shouldBe` True
    it "doesn't let bishops move through pieces" $
        isValidMove (putAt (4, 4) (Filled Black Knight) (putAt (2, 2) (Filled White Bishop) unfilledBoard)) (2, 2) (5, 5) `shouldBe` False
    it "allows bishop captures" $
        isValidMove (putAt (4, 4) (Filled Black Knight) (putAt (2, 2) (Filled White Bishop) unfilledBoard)) (2, 2) (4, 4) `shouldBe` True
    it "doesn't let kings move more that one" $ do
        isValidMove (putAt (2, 2) (Filled White King) unfilledBoard) (2, 2) (2, 1) `shouldBe` True
        isValidMove (putAt (2, 2) (Filled White King) unfilledBoard) (2, 2) (3, 3) `shouldBe` True
        isValidMove (putAt (2, 2) (Filled White King) unfilledBoard) (2, 2) (2, 4) `shouldBe` False
        isValidMove (putAt (2, 2) (Filled White King) unfilledBoard) (2, 2) (2, 0) `shouldBe` False
        isValidMove (putAt (2, 2) (Filled White King) unfilledBoard) (2, 2) (1, 2) `shouldBe` True
    it "lets queens move in diagonals/straight" $ do
        isValidMove (putAt (2, 2) (Filled White Queen) unfilledBoard) (2, 2) (4, 4) `shouldBe` True
        isValidMove (putAt (2, 2) (Filled White Queen) unfilledBoard) (2, 2) (3, 4) `shouldBe` False
        isValidMove (putAt (2, 2) (Filled White Queen) unfilledBoard) (2, 2) (0, 4) `shouldBe` True
        isValidMove (putAt (2, 2) (Filled White Queen) unfilledBoard) (2, 2) (1, 3) `shouldBe` True
        isValidMove (putAt (2, 2) (Filled White Queen) unfilledBoard) (2, 2) (1, 3) `shouldBe` True
        isValidMove (putAt (2, 2) (Filled White Queen) unfilledBoard) (2, 2) (2, 3) `shouldBe` True
        isValidMove (putAt (2, 2) (Filled White Queen) unfilledBoard) (2, 2) (2, 5) `shouldBe` True
        isValidMove (putAt (2, 2) (Filled White Queen) unfilledBoard) (2, 2) (4, 2) `shouldBe` True
        isValidMove (putAt (2, 2) (Filled White Queen) unfilledBoard) (2, 2) (0, 2) `shouldBe` True
        isValidMove (putAt (2, 2) (Filled White Queen) unfilledBoard) (2, 2) (2, 1) `shouldBe` True
        isValidMove (putAt (2, 2) (Filled White Queen) unfilledBoard) (2, 2) (1, 1) `shouldBe` True
        isValidMove (putAt (2, 2) (Filled White Queen) unfilledBoard) (2, 2) (5, 1) `shouldBe` False
        isValidMove (putAt (2, 2) (Filled White Queen) unfilledBoard) (2, 2) (3, 1) `shouldBe` True
    it "lets pawn move forward" $ do
        isValidMove ((putAt (2, 2) (Filled White Pawn) unfilledBoard)) (2, 2) (3, 2) `shouldBe` True
        isValidMove ((putAt (2, 2) (Filled Black Pawn) unfilledBoard)) (2, 2) (1, 2) `shouldBe` True
    it "doesn't let pawn capture forward" $ do
        isValidMove (putAt (3, 2) (Filled Black Pawn) (putAt (2, 2) (Filled White Pawn) unfilledBoard)) (2, 2) (3, 2) `shouldBe` False
        isValidMove (putAt (1, 2) (Filled White Pawn) (putAt (2, 2) (Filled Black Pawn) unfilledBoard)) (2, 2) (1, 2) `shouldBe` False
    it "doesn't let pawn move backward" $ do
        isValidMove ((putAt (2, 2) (Filled White Pawn) unfilledBoard)) (2, 2) (1, 2) `shouldBe` False
        isValidMove ((putAt (2, 2) (Filled Black Pawn) unfilledBoard)) (2, 2) (3, 2) `shouldBe` False
    it "lets pawn capture forward diagonal" $ do
        isValidMove (putAt (3, 3) (Filled Black Pawn) (putAt (2, 2) (Filled White Pawn) unfilledBoard)) (2, 2) (3, 3) `shouldBe` True
        isValidMove (putAt (1, 3) (Filled White Pawn) (putAt (2, 2) (Filled Black Pawn) unfilledBoard)) (2, 2) (1, 3) `shouldBe` True
        isValidMove (putAt (3, 3) (Filled White Pawn) (putAt (2, 2) (Filled Black Pawn) unfilledBoard)) (2, 2) (3, 3) `shouldBe` False
        isValidMove (putAt (1, 3) (Filled Black Pawn) (putAt (2, 2) (Filled White Pawn) unfilledBoard)) (2, 2) (1, 3) `shouldBe` False

        
            