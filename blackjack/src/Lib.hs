module Lib
    (
    startMoney,
    cardScore,
    genCardDeck,
    shuffleDeck,
    cardToString,
    getCard,
    getHand,
    removeTopCard,
    hitMove,
    getHandInt,
    baseScore,
    scoreHand,
    checkIfLegalBet,
    keepPlayingOrNot,
    Card(..)
    ) where

import Control.Monad.Random
import qualified System.Random as Rand
import System.Random.Shuffle (shuffleM)
import Data.Char(toUpper)
import Data.Bool (Bool(False))

data Card =
    Two | Three | Four | Five |
    Six | Seven | Eight | Nine |
    Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Enum)

-- | startMoney - The amount of money the player starts with in their bank
startMoney :: Int
startMoney = 10000

-- Gets the card value
-- | cardScore
-- >>> cardScore Two
-- 2
-- >>> cardScore Queen
-- 10
-- >>> cardScore Ace
-- 1
cardScore :: Card -> Int
cardScore Two = 2
cardScore Three = 3
cardScore Four = 4
cardScore Five = 5
cardScore Six = 6
cardScore Seven = 7
cardScore Eight = 8
cardScore Nine = 9
cardScore Ten = 10
cardScore Jack = 10
cardScore Queen = 10
cardScore King = 10
cardScore Ace = 1


-- Gets the card value
-- | cardScore
-- >>> cardToString Two
-- "2"
-- >>> cardToString Queen
-- "Q"
-- >>> cardToString Ace
-- "A"
cardToString :: Card -> String
cardToString Two = "2"
cardToString Three = "3"
cardToString Four = "4"
cardToString Five = "5"
cardToString Six = "6"
cardToString Seven = "7"
cardToString Eight = "8"
cardToString Nine = "9"
cardToString Ten = "10"
cardToString Jack = "J"
cardToString Queen = "Q"
cardToString King = "K"
cardToString Ace = "A"

-- Generates a 52x4 card deck to be used in the game
-- | genCardDeck
-- >>> genCardDeck
-- [Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King,Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King,Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King,Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King,Ace]
genCardDeck :: [Card]
genCardDeck = concat $ replicate 4 fullDeck
    where
        fullDeck =  [ Two, Three, Four, Five,
                      Six, Seven, Eight, Nine,
                      Ten, Jack, Queen, King, Ace
                    ]

-- Shuffles the deck to make it random based on seed
shuffleDeck :: Rand.StdGen -> ([Card], Rand.StdGen)
shuffleDeck gen = runRand (shuffleM genCardDeck) gen



getCard :: StdGen -> Card
getCard generator = genCardDeck !! rand where
    n = length genCardDeck
    (rand, _) = randomR (0,(n-1)) generator


-- Gets the hand in the form of a string so it can be shown in terminal
-- | getHand
-- >>> getHand [Ace, Two, Three]
-- "A 2 3 "
getHand :: [Card] -> String
getHand currHand = concat [cardToString card ++ " " | card <- currHand]


-- Gets the hand in the form of a Int so it can be shown in terminal
-- | getHandInt
-- >>> getHandInt [Ace, Two, Three]
-- 6
getHandInt :: [Card] -> Int
getHandInt currHand = sum (map cardScore currHand)

-- Removes the top card of the deck
-- | removeTopCard 
-- >>> removeTopCard [Ace, Two, Three]
-- [Two,Three]
removeTopCard :: [Card] -> [Card]
removeTopCard newDeck = tail newDeck


-- Gets a new card into hand and removes top card from deck
-- | hitMove
-- >>> hitMove ([Two, Three, Four], [Five, Six])
-- ([Three,Four],[Two,Five,Six])
hitMove :: ([Card], [Card]) -> ([Card], [Card])
hitMove (deck, hand) = (newDeck, newHand)
    where
        newHand = [head deck] ++ hand
        newDeck = removeTopCard deck


-- Returns a bool depending on if input is N or something else
-- | keepPlayingOrNot
-- >>> keepPlayingOrNot "N"
-- True
-- >>> keepPlayingOrNot "Heihei"
-- False
keepPlayingOrNot :: [Char] -> Bool
keepPlayingOrNot keepPlaying = toUpper (head keepPlaying) == 'N'



-- Returns the base sum, as well as a boolean if we have
-- a "usable" Ace.
-- | baseScore
-- >>> baseScore [Ace, Ace]
-- (2,True)
-- >>> baseScore [Queen, Seven]
-- (17,False)
baseScore :: [Card] -> (Int, Bool)
baseScore cards = (score, score <= 11 && Ace `elem` cards)
  where
    score = sum (cardScore <$> cards)


-- Gets the score of the hand as an Int
-- | scoreHand
-- >>> scoreHand [Queen, Seven]
-- 17
-- >>> scoreHand [Eight]
-- 8
scoreHand :: [Card] -> Int
scoreHand cards = if hasUsableAce then score + 10 else score
  where
    (score, hasUsableAce) = baseScore cards


-- Checks if bet is lower than bank account making it legal to bet the amount
-- | checkIfLegalBet
-- >>> checkIfLegalBet 10 20
-- False
-- >>> checkIfLegalBet 10000 10000
-- True
-- >>> checkIfLegalBet 5 0
-- False
checkIfLegalBet :: Int -> Int -> Bool
checkIfLegalBet 0 _ = False
checkIfLegalBet _ 0 = False
checkIfLegalBet bank bet = bet <= bank

