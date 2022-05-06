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

    startRound startMoney

    -- Get new seed which makes deck random
    --seed <- newStdGen

    --let (newDeck, newGen) = shuffleDeck seed -- Get start deck
    --let startHand = getCard seed :[] -- Get starting hand

    -- Need to create way to take OneRandCard value out of cardDeck

    --gameloop $ startRound startMoney False
    putStrLn "Welcome to Vegas Blackjack PROG2006 Special Edition!"


    --let hei = concat [cardToString card ++ " " | card <- genCardDeck]
    --putStrLn cardDeckString
    --putStrLn ("In order: " ++ hei)
    -- putStrLn hand



--------------------------------------
-- Impure functions below this line --
--------------------------------------

gameloop :: [Card] -> [Card] -> Int -> Int -> Bool -> IO ()
gameloop cardDeck currHand bankAccount bettingAmount gameDone
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
        let newScore = scoreHand newHand
        putStrLn $ "Value in hand: " ++ show newScore


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
        gameloop newDeck newHand bankAccount bettingAmount handOverLegal
    | gameDone = 
        putStrLn "Game Over"
    | otherwise = putStrLn "You messed something up and crashed the game buddy"


startRound :: Int -> IO ()
startRound bank = do
    seed <- newStdGen

    (newBankValue ,bettingAmount) <- getBettingAmount bank

    let (newDeck, newGen) = shuffleDeck seed -- Get start deck
    let firstCard = [getCard seed] -- Get starting hand
    let startHand = playerHit firstCard newDeck

    --return (newDeck, startHand, newBankValue, bettingAmount)
    putStrLn "Best of luck player!"
    gameloop newDeck startHand newBankValue bettingAmount False


getBettingAmount :: Int -> IO (Int, Int)
getBettingAmount bankAccount = do
    putStrLn $ "\nYour bank account: " ++ show bankAccount
    putStrLn "\nHow much would you like to bet?"
    bettingAmount <- getLine
    let bet = read bettingAmount

    if checkIfLegalBet bankAccount bet then
        return (bankAccount-bet, bet)
    else do
        getBettingAmount bankAccount
