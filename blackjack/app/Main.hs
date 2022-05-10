{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Lib
import qualified System.Random as Rand
import System.Random
import Data.Char(digitToInt)
import Control.Monad.IO.Class
import Lib(Card(..))
import Control.Exception (handle)

type GameState = ([Card], [Card], [Card], Int, Int)


startMoney :: Int
startMoney = 10000

main :: IO ()
main = do
    putStrLn "Welcome to Vegas Blackjack PROG2006 Special Edition!"

    startRound startMoney


--------------------------------------
-- Impure functions below this line --
--------------------------------------

gameloop :: GameState -> Bool -> IO ()
gameloop (cardDeck, currHand, dealerHand, bankAccount, bet) gameDone
    | not gameDone = do
        putStrLn "-----------------------------New Game Round-----------------------------"
        let (newDeck, newHand) = hitMove (cardDeck, currHand)
        --putStrLn $ getHand cardDeck
        putStrLn $ "Your hand: " ++ getHand newHand
        let score = scoreHand newHand
        putStrLn $ "Value in hand: " ++ show score

        putStrLn $ "\nDealer hand: " ++ getHand dealerHand
        putStrLn $ "Dealer hand value: " ++ show (scoreHand dealerHand) ++ "\n"

        if score > 21 then do
            if bankAccount > 0 then do
                putStrLn "\nYou lost this round and your bet!\nDo you want to keep playing? (Y),(N)"
                keepPlaying <- getLine
                let quitGame = keepPlayingOrNot keepPlaying

                --seed <- newStdGen
                --let (newSeededDeck, _) = shuffleDeck seed -- Get start deck
                if quitGame then 
                    gameloop (newDeck, newHand, dealerHand, bankAccount-bet, bet) True
                else do
                    putStrLn "-----------------------------New Game-----------------------------"
                    startRound bankAccount
                --gameloop (newSeededDeck, newHand, dealerHand, bankAccount-bet, bet) quitGame
            else do
                putStrLn "\nYou lost this round and your bet!\nYou are unfortunatly broke and got to go home now :(\n\n"
                gameloop (newDeck, newHand, dealerHand, bankAccount-bet, bet) True
        else do
            putStrLn "Player moves: Hit(1), Stand(2), Double Down(3), Split Pairs(4)"
            choice <- getLine
            hitStandDoubleOrSplit choice (newDeck, newHand, dealerHand, bankAccount, bet)


    | gameDone = 
        putStrLn "Game Over"
    | otherwise = putStrLn "You messed something up and crashed the game buddy"


startRound :: Int -> IO ()
startRound bank = do
    if bank <= 0 then gameloop ([], [], [], bank, 0) True
    else do 
        seed <- newStdGen

        (newBankValue ,bettingAmount) <- getBettingAmount bank

        let (seededDeck, newGen) = shuffleDeck seed -- Get start deck
        --putStrLn $ "seeddeck: " ++ getHand seededDeck

        let (deck, playerHand) = hitMove (seededDeck, [])--[getCard seed] -- Get starting hand

        let (deckAfterDraws, firstCardDealer) = hitMove (deck, [])--[getCard seed] -- Get starting hand
    -- putStrLn $ "CardDeck: " ++ getHand deck

        putStrLn "Best of luck player!"
        gameloop (deckAfterDraws, playerHand, firstCardDealer, newBankValue, bettingAmount) False


getBettingAmount :: Int -> IO (Int, Int)
getBettingAmount bankAccount = do
    putStrLn $ "\nYour bank account: $" ++ show bankAccount
    putStrLn "\nHow much would you like to bet?"
    bettingAmount <- getLine
    let bet = read bettingAmount

    if checkIfLegalBet bankAccount bet then
        return (bankAccount-bet, bet)
    else do
        getBettingAmount bankAccount


hitStandDoubleOrSplit :: [Char] -> GameState -> IO ()
hitStandDoubleOrSplit choice (deck, hand, dealerHand, bankAccount, bet) 
    | choice == "1" = gameloop (deck, hand, dealerHand, bankAccount, bet) False
    | choice == "2" = dealerHit (deck, hand, dealerHand, bankAccount, bet)
    | choice == "3" = putStrLn "Hei 3"
    | choice == "4" = putStrLn "Hei 4"
    | otherwise = putStrLn "Hei 5"


{- hit :: GameState -> IO ()
hit (deck, hand, dealerHand, bankAccount, bet) = do
    --putStr "\ESC[2J\ESC[2J\n" -- Clears the screen/ terminal
    let (newDeck, newHand) = hitMove (deck, hand)
    --let newDeck = removeTopCard deck
    putStrLn "\n-----------------------------Player Hit-----------------------------"

    putStrLn $ "Hand after hit: " ++ getHand newHand
    let newScore = scoreHand newHand
    putStrLn $ "Value in hand: " ++ show newScore

    --let handOverLegal = checkIfOverLegalValue newScore
    if newScore > 21 then do
        if bankAccount-bet > 0 then do
            putStrLn "\nYou lost this round and your bet!\nDo you want to keep playing? (Y),(N)"
            keepPlaying <- getLine
            let quitGame = keepPlayingOrNot keepPlaying
            gameloop (newDeck, newHand, dealerHand, bankAccount-bet, bet) quitGame
        else do
            putStrLn "\nYou lost this round and your bet!\nYou are unfortunatly broke and got to go home now :(\n\n"
            gameloop (newDeck, newHand, dealerHand, bankAccount-bet, bet) True
    else do
        putStrLn "Player moves: Hit(1), Stand(2), Double Down(3), Split Pairs(4)"
        choice <- getLine
        hitStandDoubleOrSplit choice (newDeck, newHand, dealerHand, bankAccount, bet)

 -}

dealerHit :: GameState -> IO ()
dealerHit (deck, hand, dealerHand, bankAccount, bet) = do
    let (newDeck, dealerHitHand) = hitMove (deck, dealerHand)
    let newScore = scoreHand dealerHitHand
    putStrLn $ "Dealer hand after hit: " ++ getHand dealerHitHand
    putStrLn $ "Value in hand: " ++ show newScore

    dealerAi (newDeck, hand, dealerHitHand, bankAccount, bet)


dealerAi :: GameState -> IO ()
dealerAi (deck, hand, dealerHand, bankAccount, bet)
    | scoreHand dealerHand > 21 = do
        putStrLn "\nYou won!"
        startRound (bankAccount+(bet*2))
    | scoreHand dealerHand < 17 = dealerHit (deck, hand, dealerHand, bankAccount, bet)
    | scoreHand dealerHand < scoreHand hand = dealerHit (deck, hand, dealerHand, bankAccount, bet)
    | scoreHand dealerHand == scoreHand hand = do
        putStrLn "\nDRAW! Money returned to your account."
        startRound (bankAccount+bet)
    | scoreHand dealerHand > scoreHand hand = do
        putStrLn $ "\nDealer won!\n$" ++ show bet ++ " lost from your account."
        startRound bankAccount
    | otherwise = do
        putStrLn "\nSomething weird happened, restarting game"
        startRound (bankAccount+bet)
