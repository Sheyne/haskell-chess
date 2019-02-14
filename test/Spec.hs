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
