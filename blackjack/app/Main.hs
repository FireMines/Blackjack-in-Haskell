module Main where

import Lib
import qualified System.Random as Rand
import System.Random
import Data.Char(digitToInt)

import Lib(Card(..))

startMoney = 10000

main :: IO ()
main = do
    putStrLn "Welcome to Vegas Blackjack PROG2006 Special Edition!"

    -- Get new seed which makes deck random
    seed <- newStdGen

    let (newDeck, newGen) = shuffleDeck seed -- Get start deck
    let startHand = getCard seed :[] -- Get starting hand

    -- Need to create way to take OneRandCard value out of cardDeck

    gameloop newDeck startHand startMoney False

    --let hei = concat [cardToString card ++ " " | card <- genCardDeck]
    --putStrLn cardDeckString
    --putStrLn ("In order: " ++ hei)
    -- putStrLn hand



--------------------------------------
-- Impure functions below this line --
--------------------------------------

gameloop :: [Card] -> [Card] -> Int -> Bool -> IO ()
gameloop cardDeck currHand bankAccount gameDone
    | not gameDone = do
        putStrLn $ getHand cardDeck
        putStrLn $ "Your hand: " ++ getHand currHand
        let newDeck = removeTopCard cardDeck
        putStrLn $ "CardDeck: " ++ getHand newDeck
        putStrLn "Player moves: Hit, Stand, Double Down, Split Pairs"
        let newHand = playerHit currHand newDeck
        putStrLn $ "Hand after hit: " ++ getHand newHand
        --if sum $ digitToInt newHand < 21 then do
        --    putStrLn "Hei"
        --    putStrLn "Paa deg"
        --else do 
        --    putStrLn "Nei"
        --let hei = checkIfOverLegalValue newHand
        gameloop cardDeck currHand bankAccount True
    | gameDone = 
        putStrLn "Game Over"
    | otherwise = putStrLn "You messed something up and crashed the game buddy"