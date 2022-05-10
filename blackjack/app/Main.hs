{-# LANGUAGE BlockArguments #-}
module Main where

import Lib
import qualified System.Random as Rand
import System.Random
import Data.Char(digitToInt)
import Control.Monad.IO.Class

import Lib(Card(..))
import Control.Exception (handle)

type GameState = ([Card], [Card], [Card], Int, Int)


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
    --putStrLn "Welcome to Vegas Blackjack PROG2006 Special Edition!"


    --let hei = concat [cardToString card ++ " " | card <- genCardDeck]
    --putStrLn cardDeckString
    --putStrLn ("In order: " ++ hei)
    -- putStrLn hand



--------------------------------------
-- Impure functions below this line --
--------------------------------------

gameloop :: GameState -> Bool -> IO ()
gameloop (cardDeck, currHand, dealerHand, bankAccount, bettingAmount) gameDone
    | not gameDone = do
        putStrLn "-----------------------------New Game Round-----------------------------"
        --putStrLn $ getHand cardDeck
        putStrLn $ "Your hand: " ++ getHand currHand
        let score = scoreHand currHand
        putStrLn $ "Value in hand: " ++ show score

        putStrLn $ "\nDealer hand: " ++ getHand dealerHand
        putStrLn $ "Dealer hand value: " ++ show (scoreHand dealerHand) ++ "\n"

        let newDeck = removeTopCard cardDeck
        --putStrLn $ "CardDeck: " ++ getHand newDeck

        putStrLn "Player moves: Hit(1), Stand(2), Double Down(3), Split Pairs(4)"
        choice <- getLine
        hitStandDoubleOrSplit choice (newDeck, currHand, dealerHand, bankAccount, bettingAmount)
        --(newDec, newHand, newDealerHand, newBankAccount, newBet) <- hitStandDoubleOrSplit choice (newDeck, currHand, dealerHand, bankAccount, bettingAmount)
        --putStrLn $ "Hand after hit: " ++ getHand newHand
        --let newScore = scoreHand newHand
        --putStrLn $ "Value in hand: " ++ show newScore


        --let handOverLegal = checkIfOverLegalValue newHand
        --if handOverLegal
            --then do
                --if bankAccount > 0
                    --then do 
                        --liftIO $ putStrLn "You win this round!"
                        --startRound bankAccount
                    --else liftIO $ putStrLn "You lost :("
               -- else
        --gameloop (newDeck, newHand, dealerHand, bankAccount, bettingAmount) False
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
        --gameloop newDeck newHand bankAccount bettingAmount handOverLegal
    | gameDone = 
        putStrLn "Game Over"
    | otherwise = putStrLn "You messed something up and crashed the game buddy"


startRound :: Int -> IO ()
startRound bank = do
    seed <- newStdGen

    (newBankValue ,bettingAmount) <- getBettingAmount bank

    let (newDeck, newGen) = shuffleDeck seed -- Get start deck
    let firstCard = [getCard seed] -- Get starting hand
    let startHand = hitMove firstCard newDeck

    let firstCardDealer = [getCard seed] -- Get starting hand

    --return (newDeck, startHand, newBankValue, bettingAmount)
    putStrLn "Best of luck player!"
    gameloop (newDeck, startHand, firstCardDealer, newBankValue, bettingAmount) False


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


hitStandDoubleOrSplit :: [Char] -> GameState -> IO ()
hitStandDoubleOrSplit choice (deck, hand, dealerHand, bankAccount, bet) 
    | choice == "1" = hit (deck, hand, dealerHand, bankAccount, bet)
    | choice == "2" = dealerHit (deck, hand, dealerHand, bankAccount, bet)
    | choice == "3" = putStrLn "Hei 3"
    | choice == "4" = putStrLn "Hei 4"
    | otherwise = putStrLn "Hei 5"


hit :: GameState -> IO ()
hit (deck, hand, dealerHand, bankAccount, bet) = do
    let newHand = hitMove hand deck
    let newDeck = removeTopCard deck

    putStrLn $ "\nHand after hit: " ++ getHand newHand
    let newScore = scoreHand newHand
    putStrLn $ "Value in hand: " ++ show newScore

    --let handOverLegal = checkIfOverLegalValue newScore
    if newScore > 21
        then do
            if bankAccount-bet > 0
                then do
                    putStrLn "You lost this round and your bet!\nDo you want to keep playing? (Y),(N)"
                    keepPlaying <- getLine
                    let newRound = keepPlayingOrNot keepPlaying
                    gameloop (newDeck, newHand, dealerHand, bankAccount-bet, bet) newRound

                    --startRound bankAccount
                else do 
                    putStrLn "You lost this round and your bet!\nYou are unfortunatly broke and got to go home now :("
                    gameloop (newDeck, newHand, dealerHand, bankAccount-bet, bet) True
            else gameloop (newDeck, newHand, dealerHand, bankAccount-bet, bet) True
            --gameloop newDeck newHand bankAccount bet handOverLegal   


dealerHit :: GameState -> IO ()
dealerHit (deck, hand, dealerHand, bankAccount, bet) = do
    let dealerHitHand = hitMove dealerHand deck
    let newScore = scoreHand dealerHitHand
    putStrLn $ "Dealer hand after hit: " ++ getHand dealerHitHand
    putStrLn $ "Value in hand: " ++ show newScore

    dealerAi (deck, hand, dealerHitHand, bankAccount, bet)


dealerAi :: GameState -> IO ()
dealerAi (deck, hand, dealerHand, bankAccount, bet)
    | scoreHand dealerHand < 17 = dealerHit (deck, hand, dealerHand, bankAccount, bet)
    | scoreHand dealerHand > 21 = gameloop (deck, hand, dealerHand, bankAccount, bet) False
    | scoreHand dealerHand < scoreHand hand = dealerHit (deck, hand, dealerHand, bankAccount, bet)
    | otherwise = gameloop (deck, hand, dealerHand, bankAccount, bet) False
