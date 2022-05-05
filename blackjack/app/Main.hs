module Main where

import Lib
import qualified System.Random as Rand
import System.Random
import Data.Char(digitToInt)

import Lib(Card(..))
import Control.Exception (handle)

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
        putStrLn "-----------------------------New Game Round-----------------------------"
        --putStrLn $ getHand cardDeck
        putStrLn $ "Your hand: " ++ getHand currHand
        let score = scoreHand currHand
        putStrLn $ "Value in hand: " ++ show score

        let newDeck = removeTopCard cardDeck
        --putStrLn $ "CardDeck: " ++ getHand newDeck
        putStrLn "Player moves: Hit(1), Stand(2), Double Down(3), Split Pairs(4)"
        choice <- getLine
        
        let newHand = hitStandDoubleOrSplit choice newDeck currHand
        putStrLn $ "Hand after hit: " ++ getHand newHand


        let handOverLegal = checkIfOverLegalValue newHand
        --if length cardDeck == 0 then do
            --let noCardsLeft = True
        --    putStrLn "No cards left"
        --    gameloop newDeck newHand bankAccount True
        --putStrLn $ "Blabla " ++ getHandInt newHand
        --putStrLn $ "Sum: " ++ show (getHandInt newHand)
        --else do
            --let noCardsLeft = False
        --    putStrLn "Still some cards left"
        --    gameloop newDeck newHand bankAccount False
        gameloop newDeck newHand bankAccount handOverLegal
    | gameDone = 
        putStrLn "Game Over"
    | otherwise = putStrLn "You messed something up and crashed the game buddy"


