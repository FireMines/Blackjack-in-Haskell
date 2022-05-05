module Lib
    ( 
    cardScore,
    genCardDeck,
    shuffleDeck,
    cardToString,
    getCard,
    getHand,
    removeTopCard,
    playerHit,
    --hand,
    Card(..)
    ) where

import Control.Monad.Random
import qualified System.Random as Rand
import System.Random.Shuffle (shuffleM)

data Card = 
    Two | Three | Four | Five | 
    Six | Seven | Eight | Nine | 
    Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Enum)


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
-- "10"
-- >>> cardToString Ace
-- "1"
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


genCardDeck :: [Card]
genCardDeck = concat $ replicate 4 fullDeck
    where
        fullDeck =  [ Two, Three, Four, Five,
                      Six, Seven, Eight, Nine,
                      Ten, Jack, Queen, King, Ace
                    ]


shuffleDeck :: Rand.StdGen -> ([Card], Rand.StdGen)
shuffleDeck gen = runRand (shuffleM genCardDeck) gen


getCard :: StdGen -> Card
getCard generator = genCardDeck !! rand where
    n = length genCardDeck
    (rand, _) = randomR (0,(n-1)) generator


getHand :: [Card] -> String
getHand currHand = concat [cardToString card ++ " " | card <- currHand]


--usp :: Card -> [Int]
--usp a [] = [a]
--usp a (x:xs) = x : hand (cardScore a) xs


removeTopCard :: [Card] -> [Card]
removeTopCard newDeck = tail newDeck


playerHit :: [Card] -> [Card] -> [Card]
playerHit hand deck = [head deck] ++ hand


--checkIfOverLegalValue :: [Card] -> Bool
--checkIfOverLegalValue currHand
--    | sum $ digitToInt currHand < 21 == False
--    | sum $ digitToInt currHand > 21 == True
--    | concat [cardScore card ++ " " | card <- currHand] < 21 = False
--    | concat [cardScore card ++ " " | card <- currHand] > 21 = True
--    | concat [cardScore card ++ " " | card <- currHand] == 21 = 
--    | otherwise = putStrLn True