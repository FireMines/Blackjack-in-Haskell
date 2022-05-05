module Main where

import Lib
import qualified System.Random as Rand
import System.Random



main :: IO ()
main = do
    putStrLn "Welcome to Vegas Blackjack PROG2006 Special Edition!"

    -- Get new seed which makes deck random
    seed <- newStdGen

    --gen <- Rand.getStdGen
    let (newDeck, newGen) = shuffleDeck seed
    let cardDeckStr = concat [cardToString card ++ " " | card <- cardDeck]
    let oneRandCard = getCard seed
    let hand = cardToString oneRandCard

    -- Need to create way to take OneRandCard value out of cardDeck

    putStrLn cardDeckStr
    putStrLn hand



--------------------------------------
-- Impure functions below this line --
--------------------------------------

