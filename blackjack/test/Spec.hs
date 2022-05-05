import Test.DocTest(doctest)
import Test.HUnit(Test(TestCase, TestList, TestLabel), assertEqual, runTestTT)
import Test.QuickCheck(quickCheckAll)
import Test.Hspec(Spec, hspec, describe, shouldBe, it, context)
import Test.Hspec.QuickCheck(prop)

import Lib
--import Lib (boardToString)

spec_boardToString :: Spec
spec_boardToString = do
    describe "boardToString" $ do
        it "Converts the board to a single string so its easier to write with a putStrLn" $ do
            boardToString ["_", "_", "_", "_", "_", "_", "_", "_", "_"] `shouldBe` ("\n#___\n#___\n#___" :: String)
            boardToString ["X", "O", "X", "O", "_", "_", "_", "_", "_"] `shouldBe` ("\n#XOX\n#O__\n#___" :: String)

        context "if list is empty" $ do
            it "returns an empty string" $ do
                boardToString [] `shouldBe` ("" :: String)


spec_updateBoard :: Spec
spec_updateBoard = do 
    describe "updateBoard" $ do
        it "Updates the board with the new input" $ do
            updateBoard ["_", "_", "_", "_", "_", "_", "_", "_", "_"] 0 "X" `shouldBe` (["X","_","_","_","_","_","_","_","_"] :: [String])
            updateBoard ["X", "O", "X", "O", "_", "_", "_", "_", "_"] 8 "O" `shouldBe` (["X","O","X","O","_","_","_","_","O"] :: [String])


spec_make2DList :: Spec
spec_make2DList = do
    describe "make2DList" $ do
        it "Converts the board to a single string so its easier to write with a putStrLn" $ do
            make2DList 3 ["_", "_", "_", "_", "_", "_", "_", "_", "_"] `shouldBe` ([["_","_","_"],["_","_","_"],["_","_","_"]] :: [[String]])
            make2DList 3 ["X", "O", "X", "O", "_", "_", "_", "_", "_"] `shouldBe` ([["X","O","X"],["O","_","_"],["_","_","_"]] :: [[String]])

        context "if list is empty" $ do
            it "returns a n long empty 2d string" $ do
                make2DList 3 [] `shouldBe` ([["_","_","_"],["_","_","_"],["_","_","_"]] :: [[String]])    
                make2DList 1 [] `shouldBe` ([["_"]] :: [[String]]) 


spec_won :: Spec
spec_won = do
    describe "won" $ do
        it "Checks if a player has won or not" $ do
            won [["_","_","_"],["_","_","_"],["_","_","_"]] "X" `shouldBe` (False :: Bool)
            won [["X","O","X"],["O","_","_"],["_","_","_"]] "O" `shouldBe` (False :: Bool)
            won [["X","O","X"],["O","X","O"],["X","O","X"]] "O" `shouldBe` (True :: Bool)

spec_swapCorner :: Spec
spec_swapCorner = do
    describe "swapCorner" $ do
        it "Swaps the corner of index 0 with index 2" $ do
            swapCorner ["X", "_", "_", "_", "_", "_", "_", "_", "_"] `shouldBe` (["_","_","X","_","_","_","_","_","_"] :: [String])
            swapCorner ["X", "O", "X", "O", "_", "_", "_", "_", "_"] `shouldBe` (["X","O","X","O","_","_","_","_","_"] :: [String])

spec_rollLeft :: Spec
spec_rollLeft = do
    describe "rollLeft" $ do
        it "Makes the board roll to the left" $ do
            rollLeft ["X", "_", "_", "_", "_", "_", "_", "_", "_"] `shouldBe` (["X","_","_","_","_","_","_","_","_"] :: [String])
            rollLeft ["X", "O", "X", "O", "_", "_", "_", "_", "_"] `shouldBe` (["X","_","_","O","_","_","X","O","_"] :: [String]) 


spec_rollRight :: Spec
spec_rollRight = do
    describe "rollRight" $ do
        it "Makes the board roll to the right" $ do
            rollRight ["X", "_", "_", "_", "_", "_", "_", "_", "_"] `shouldBe` (["_","_","_","_","_","_","_","_","X"] :: [String])
            rollRight ["X", "O", "X", "O", "_", "_", "_", "_", "_"] `shouldBe` (["_","O","X","_","_","O","_","_","X"] :: [String])     

spec_rollOrKeepBoard :: Spec
spec_rollOrKeepBoard = do
    describe "rollOrKeepBoard" $ do
        it "Rolls board to left or right depending on input, if N then does not roll" $ do
            rollOrKeepBoard "R" ["_", "_", "_", "_", "_", "_", "_", "_", "_"] 5 "X" `shouldBe` (["_","_","_","_","_","_","_","X","_"] :: [String])
            rollOrKeepBoard "L" ["X", "O", "X", "O", "_", "_", "_", "_", "_"] 4 "O" `shouldBe` (["X","_","_","O","O","_","X","O","_"] :: [String])
            rollOrKeepBoard "N" ["X", "O", "X", "O", "_", "_", "_", "_", "_"] 4 "O" `shouldBe` (["X","O","X","O","O","_","_","_","_"] :: [String])


        context "if input an illegal rotation or not N" $ do
            it "returns board as if said No" $ do
                rollOrKeepBoard "HABBO" ["X", "O", "X", "O", "_", "_", "_", "_", "_"] 8 "X" `shouldBe` (["X","O","X","O","_","_","_","_","X"] :: [String])


spec_aiRotateOrKeepBoard :: Spec 
spec_aiRotateOrKeepBoard = do
    describe "aiRotateOrKeepBoard" $ do
        it "Returns R for right L for left or N for No if it should roll or not" $ do
            aiRotateOrKeepBoard 0 `shouldBe` ("R" :: String)
            aiRotateOrKeepBoard 1 `shouldBe` ("L" :: String)
            aiRotateOrKeepBoard 2 `shouldBe` ("N" :: String)


        context "if input is another than the 3 specified" $ do
            it "returns as if said No" $ do
                aiRotateOrKeepBoard 1000        `shouldBe` ("N" :: String)
                aiRotateOrKeepBoard 23452342    `shouldBe` ("N" :: String)


testBoardToString :: Test 
testBoardToString = TestCase (assertEqual "List of all _ should print all _ to screen with new lines" "\n#___\n#___\n#___" (boardToString ["_", "_", "_", "_", "_", "_", "_", "_", "_"]))

testUpdateBoard :: Test 
testUpdateBoard = TestCase (assertEqual "Updates board with mark on index" ["X","O","X","O","_","_","_","_","O"] (updateBoard ["X", "O", "X", "O", "_", "_", "_", "_", "_"] 8 "O"))

testMake2DList :: Test 
testMake2DList = TestCase (assertEqual "Makes a 2D list of input list" [["X","O","X"],["O","_","_"],["_","_","_"]] (make2DList 3 ["X", "O", "X", "O", "_", "_", "_", "_", "_"]))

testWon :: Test 
testWon = TestCase (assertEqual "Checks if game is won" True (won [["X","O","X"],["O","X","O"],["X","O","X"]] "O"))

testSwapCorner :: Test 
testSwapCorner = TestCase (assertEqual "Swaps corners on index 0 and 2" ["X","O","X","O","_","_","_","_","_"] (swapCorner ["X", "O", "X", "O", "_", "_", "_", "_", "_"]))

testRollLeft :: Test 
testRollLeft = TestCase (assertEqual "Rolls the whole board left" ["X","_","_","_","_","_","_","_","_"] (rollLeft ["X", "_", "_", "_", "_", "_", "_", "_", "_"]))

testRollRight :: Test 
testRollRight = TestCase (assertEqual "Rolls the whole board left" ["X","_","_","O","_","_","X","O","_"] (rollLeft ["X", "O", "X", "O", "_", "_", "_", "_", "_"]))

testRollOrKeepBoard :: Test 
testRollOrKeepBoard = TestCase (assertEqual "Rolls the whole board right, left or not at all" ["X","_","_","O","O","_","X","O","_"] (rollOrKeepBoard "L" ["X", "O", "X", "O", "_", "_", "_", "_", "_"] 4 "O"))

testAiRotateOrKeepBoard :: Test 
testAiRotateOrKeepBoard = TestCase (assertEqual "Checks if ai chose to roll left right or not at all" "L" (aiRotateOrKeepBoard 1))


--
-- List of all unit tests
unitTests :: Test
unitTests = TestList [
  TestLabel "boardToString empty" testBoardToString,
  TestLabel "updateBoard [] 8 O" testUpdateBoard,
  TestLabel "make2DList 3 []" testMake2DList,
  TestLabel "won [] O" testWon,
  TestLabel "swapCorner []" testSwapCorner,
  TestLabel "rollLeft []" testRollLeft,
  TestLabel "rollRight []" testRollRight,
  TestLabel "testRollOrKeepBoard [] 4 O" testRollOrKeepBoard,
  TestLabel "aiRotateOrKeepBoard 1" testAiRotateOrKeepBoard
  ]



main :: IO ()
main = do
    doctest ["-isrc", "src/Lib.hs"]

    _ <- runTestTT unitTests

    hspec $ do
        spec_boardToString
        spec_updateBoard
        spec_make2DList
        spec_won
        spec_swapCorner
        spec_rollLeft
        spec_rollRight
        spec_rollOrKeepBoard
        spec_aiRotateOrKeepBoard
