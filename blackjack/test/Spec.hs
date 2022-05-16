import Test.DocTest(doctest)
import Test.HUnit(Test(TestCase, TestList, TestLabel), assertEqual, runTestTT)
import Test.QuickCheck(quickCheckAll)
import Test.Hspec(Spec, hspec, describe, shouldBe, it, context)
import Test.Hspec.QuickCheck(prop)

import Lib
--import Lib (cardScore)

spec_cardScore :: Spec
spec_cardScore = do
    describe "cardScore" $ do
        it "Converts the board to a single string so its easier to write with a putStrLn" $ do
            cardScore Two `shouldBe` (2 :: Int)
            cardScore Queen `shouldBe` (10 :: Int)
            cardScore Ace `shouldBe` (1 :: Int)

        --context "if list is empty" $ do
        --    it "returns an empty string" $ do
        --        cardScore [] `shouldBe` ("" :: String)


spec_cardToString :: Spec
spec_cardToString = do 
    describe "cardToString" $ do
        it "Updates the board with the new input" $ do
            cardToString Six `shouldBe` ("6" :: String)
            cardToString Nine `shouldBe` ("9" :: String)
            cardToString Three `shouldBe` ("3" :: String)


spec_genCardDeck :: Spec
spec_genCardDeck = do
    describe "genCardDeck" $ do
        it "Generates a 52x4 card deck to be used in the game" $ do
            genCardDeck  `shouldBe` ([Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King,Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King,Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King,Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King,Ace] :: [Card])


--spec_shuffleDeck :: Spec
--spec_shuffleDeck = do
--    describe "shuffleDeck" $ do
--        it "Checks if a player has shuffleDeck or not" $ do
--            shuffleDeck [["_","_","_"],["_","_","_"],["_","_","_"]] "X" `shouldBe` (False :: Bool)
--            shuffleDeck [["X","O","X"],["O","_","_"],["_","_","_"]] "O" `shouldBe` (False :: Bool)
--            shuffleDeck [["X","O","X"],["O","X","O"],["X","O","X"]] "O" `shouldBe` (True :: Bool)

spec_getHand :: Spec
spec_getHand = do
    describe "getHand" $ do
        it "Gets the hand in the form of a string so it can be shown in terminal" $ do
            getHand [Ace, Two, Three] `shouldBe` ("A 2 3 " :: String)
            getHand [Nine, Seven, Three] `shouldBe` ("9 7 3 " :: String)

spec_getHandInt :: Spec
spec_getHandInt = do
    describe "getHandInt" $ do
        it "Gets the hand in the form of a Int so it can be shown in terminal" $ do
            getHandInt [Ace, King, Jack] `shouldBe` (21 :: Int) 
            getHandInt [Ace, Two, Three] `shouldBe` (6 :: Int) 


spec_removeTopCard :: Spec
spec_removeTopCard = do
    describe "removeTopCard" $ do
        it "Removes the top card of the deck" $ do
            removeTopCard [Ace, Two, Three] `shouldBe` ([Two, Three] :: [Card])
            removeTopCard [King, Five, Eight] `shouldBe` ([Five, Eight] :: [Card])

spec_hitMove :: Spec
spec_hitMove = do
    describe "hitMove" $ do
        it "Gets a new card into hand and removes top card from deck" $ do
            hitMove ([Ten, Five, Four], [Five, Six]) `shouldBe` (([Five, Four], [Ten, Five, Six]) :: ([Card], [Card]))
            hitMove ([Ace, King, Four], [Queen, Six]) `shouldBe` (([King, Four], [Ace, Queen, Six]) :: ([Card], [Card]))
            hitMove ([Two, Three, Four], [Five, Six]) `shouldBe` (([Three, Four], [Two, Five, Six]) :: ([Card], [Card]))


spec_keepPlayingOrNot :: Spec 
spec_keepPlayingOrNot = do
    describe "keepPlayingOrNot" $ do
        it "Returns a bool depending on if input is N or something else" $ do
            keepPlayingOrNot "N" `shouldBe` (True :: Bool)
            keepPlayingOrNot "PROG2006" `shouldBe` (False :: Bool)
            keepPlayingOrNot "Y" `shouldBe` (False :: Bool)
            keepPlayingOrNot "n" `shouldBe` (True :: Bool)


spec_baseScore :: Spec 
spec_baseScore = do
    describe "baseScore" $ do
        it "Returns the base sum, as well as a boolean if we have" $ do
            baseScore [Ten] `shouldBe` ((10, False) :: (Int, Bool))
            baseScore [Three, Four] `shouldBe` ((7, False) :: (Int, Bool))
            baseScore [Ace, Ace] `shouldBe` ((2, True) :: (Int, Bool))


spec_scoreHand :: Spec 
spec_scoreHand = do
    describe "scoreHand" $ do
        it "Gets the score of the hand as an Int" $ do
            scoreHand [King, Jack] `shouldBe` (20 :: Int)
            scoreHand [Ten, Two] `shouldBe` (12 :: Int)
            scoreHand [Eight] `shouldBe` (8 :: Int)


spec_checkIfLegalBet :: Spec 
spec_checkIfLegalBet = do
    describe "checkIfLegalBet" $ do
        it "Checks if bet is lower than bank account making it legal to bet the amount" $ do
            checkIfLegalBet 50 50`shouldBe` (True :: Bool)
            checkIfLegalBet 100 10`shouldBe` (True :: Bool)
            checkIfLegalBet 20 200`shouldBe` (False :: Bool)


        context "if input is not defined it one of the two input areas" $ do
            it "returns as False" $ do
                checkIfLegalBet 1000 0       `shouldBe` (False :: Bool)
                checkIfLegalBet 23452342 0   `shouldBe` (False :: Bool)



testcardScore :: Test 
testcardScore = TestCase (assertEqual "Gets the card value" 5 (cardScore Five))

testcardToString :: Test 
testcardToString = TestCase (assertEqual "Gets the card value" "2" (cardToString Two))

testgenCardDeck :: Test 
testgenCardDeck = TestCase (assertEqual "Generates a 52 card deck to be used in the game" [Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King,Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King,Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King,Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King,Ace] genCardDeck)

--testshuffleDeck :: Test 
--testshuffleDeck = TestCase (assertEqual "Shuffles the deck to make it random based on seed" True (shuffleDeck 17))

testgetHand :: Test 
testgetHand = TestCase (assertEqual "Gets the hand in the form of a string so it can be shown in terminal" "A 2 3 " (getHand [Ace, Two, Three]))

testgetHandInt :: Test 
testgetHandInt = TestCase (assertEqual "Gets the hand in the form of a Int so it can be shown in terminal" 6 (getHandInt [Ace, Two, Three]))

testremoveTopCard :: Test 
testremoveTopCard = TestCase (assertEqual "Removes the top card of the deck" [Three] (removeTopCard [Two, Three]))

testhitMove :: Test 
testhitMove = TestCase (assertEqual "Gets a new card into hand and removes top card from deck" ([Three, Four], [Two, Five, Six]) (hitMove ([Two, Three, Four], [Five, Six])))

testkeepPlayingOrNot :: Test 
testkeepPlayingOrNot = TestCase (assertEqual "Returns a bool depending on if input is N or something else" False (keepPlayingOrNot "Heihei"))

testbaseScore :: Test 
testbaseScore = TestCase (assertEqual "Returns the base sum, as well as a boolean if we have" (12, False) (baseScore [Queen, Two]))

testscoreHand :: Test 
testscoreHand = TestCase (assertEqual "Gets the score of the hand as an Int" 10 (scoreHand [Ten]))

testcheckIfLegalBet :: Test 
testcheckIfLegalBet = TestCase (assertEqual "Checks if bet is lower than bank account making it legal to bet the amount" False (checkIfLegalBet 10 20))



--
-- List of all unit tests
unitTests :: Test
unitTests = TestList [
  TestLabel "cardScore" testcardScore,
  TestLabel "cardToString" testcardToString,
  TestLabel "genCardDeck" testgenCardDeck,
  --TestLabel "shuffleDeck" testshuffleDeck,
  TestLabel "getHand" testgetHand,
  TestLabel "getHandInt" testgetHandInt,
  TestLabel "removeTopCard" testremoveTopCard,
  TestLabel "testhitMove" testhitMove,
  --TestLabel "keepPlayingOrNot" testkeepPlayingOrNot
  TestLabel "baseScore" testbaseScore,
  TestLabel "scoreHand" testscoreHand,
  TestLabel "checkIfLegalBet" testcheckIfLegalBet
  ]



main :: IO ()
main = do
    doctest ["-isrc", "src/Lib.hs"]

    _ <- runTestTT unitTests

    hspec $ do
        spec_cardScore
        spec_cardToString
        spec_genCardDeck
        --spec_shuffleDeck
        spec_getHand
        spec_getHandInt
        spec_removeTopCard
        spec_hitMove
        spec_keepPlayingOrNot
        spec_baseScore
        spec_scoreHand
        spec_checkIfLegalBet
