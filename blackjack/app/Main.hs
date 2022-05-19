{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Lib
import System.Random
import System.Environment


type GameState = ([Card], [Card], [Card], Int, Int)


main :: IO ()
main = do
    flags <- getArgs
    if flags /= [] then do
        if head flags == "--h" || head flags == "--help" || head flags == "help" then do
            help
        else
            putStrLn "\ESC[2J\ESC[2J\nWelcome to Vegas Blackjack PROG2006 Special Edition!"
    else
        putStrLn "\ESC[2J\ESC[2J\nWelcome to Vegas Blackjack PROG2006 Special Edition!"

    startRound startMoney



--------------------------------------
-- Impure functions below this line --
--------------------------------------


-- | help - Prints basics of the program to the user
help :: IO ()
help = do
    putStrLn "\ESC[2J\ESC[2J\nWelcome to Vegas Blackjack PROG2006 Special Edition!"
    putStrLn "\nThis is a game of traditional blackjack which follows the rules used in casinos in Las Vegas."
    putStrLn "The goal is to beat the dealer and get as close to the value 21 in your hand. If you go above 21, you lose the money you bet."
    putStrLn "The game starts with you saying how much you would like to bet."
    putStrLn "You then get 2 cards and the dealer gets 1. You can now choose if you want to hit, stand or double."
    putStrLn "Hit means you get another card, stand means its the dealers turn, double means you hit once and double your bet."
    putStrLn "This is how the game plays on easily explained. If you win over the dealer (get lower than 21 but closer to 21 than the dealer did)"
    putStrLn "you get your bet back, as well as the same amount you bet. If you loose, you loose your bet."
    putStrLn "All you need to try to do is increase your bank account as much as you can, before the casino thrown you out!"
    putStrLn "Remember though, the house ALWAYS wins, one way or another... ;)"


-- | gameloop - Loops the main areas of the game
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

                if quitGame then
                    gameloop (newDeck, newHand, dealerHand, bankAccount-bet, bet) True
                else do
                    putStrLn "-----------------------------New Game-----------------------------"
                    startRound bankAccount
            else do
                putStrLn "\nYou lost this round and your bet!\nYou are unfortunatly broke and got to go home now :(\n\n"
                gameloop (newDeck, newHand, dealerHand, bankAccount-bet, bet) True
        else do
            putStrLn "Player moves: Hit(1), Stand(2) or Double Down(3)"
            choice <- getLine
            hitStandDoubleOrSplit choice (newDeck, newHand, dealerHand, bankAccount, bet)


    | otherwise =
        putStrLn "Game Over"


-- | startRound - first round of a new game
startRound :: Int -> IO ()
startRound bank = do
    if bank <= 0 then gameloop ([], [], [], bank, 0) True
    else do
        seed <- newStdGen

        (newBankValue ,bettingAmount) <- getBettingAmount bank

        let (seededDeck, _) = shuffleDeck seed -- Get start deck
        --putStrLn $ "seeddeck: " ++ getHand seededDeck

        let (deck, playerHand) = hitMove (seededDeck, []) -- Get starting hand

        let (deckAfterDraws, firstCardDealer) = hitMove (deck, []) -- Get starting hand
    -- putStrLn $ "CardDeck: " ++ getHand deck

        putStrLn "Best of luck player!"
        gameloop (deckAfterDraws, playerHand, firstCardDealer, newBankValue, bettingAmount) False


-- | getBettingAmount - Gets the bet from player and updates bank account
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


-- | hitStandDoubleOrSplit - Decides player moves
hitStandDoubleOrSplit :: [Char] -> GameState -> IO ()
hitStandDoubleOrSplit choice (deck, hand, dealerHand, bankAccount, bet)
    | choice == "1" = gameloop (deck, hand, dealerHand, bankAccount, bet) False
    | choice == "2" = dealerHit (deck, hand, dealerHand, bankAccount, bet)
    | choice == "3" = do
        if bankAccount - bet < 0 then do
            putStrLn "\nYou can't double because you don't got enough $$$"
            putStrLn "Player moves: Hit(1) or Stand(2)"
            newChoice <- getLine

            hitStandDoubleOrSplit newChoice (deck, hand, dealerHand, bankAccount, bet)
        else
            double (deck, hand, dealerHand, bankAccount - bet, bet * 2)
    | otherwise = do
        putStrLn "\nThat is not a legal command/ play to make. Please play by the rules and choose one of the below: "
        putStrLn "Player moves: Hit(1), Stand(2) or Double Down(3)"
        newChoice <- getLine

        hitStandDoubleOrSplit newChoice (deck, hand, dealerHand, bankAccount, bet)


-- | dealerHit - Dealer hitting if his AI tells him to do it
dealerHit :: GameState -> IO ()
dealerHit (deck, hand, dealerHand, bankAccount, bet) = do
    let (newDeck, dealerHitHand) = hitMove (deck, dealerHand)
    let newScore = scoreHand dealerHitHand
    putStrLn $ "Dealer hand after hit: " ++ getHand dealerHitHand
    putStrLn $ "Value in hand: " ++ show newScore

    dealerAi (newDeck, hand, dealerHitHand, bankAccount, bet)


-- | dealerAi - The core ai mechanisms depending on how the gamestate is
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
        putStrLn $ "\nDealer won!\n$" ++ show bet ++ " lost from your account.\n"
        startRound bankAccount
    | otherwise = do
        putStrLn "\nSomething weird happened, restarting game"
        startRound (bankAccount+bet)


-- | double - If player chooses to double the value of his bet
double :: GameState -> IO ()
double (cardDeck, currHand, dealerHand, bankAccount, bet) = do
    putStrLn "-----------------------------New Game Round-----------------------------"
    let (newDeck, newHand) = hitMove (cardDeck, currHand)
    --putStrLn $ getHand cardDeck
    putStrLn $ "Your hand: " ++ getHand newHand
    putStrLn $ "Value in hand: " ++ show (scoreHand newHand)

    putStrLn $ "\nDealer hand: " ++ getHand dealerHand
    putStrLn $ "Dealer hand value: " ++ show (scoreHand dealerHand) ++ "\n"

    dealerHit (newDeck, newHand, dealerHand, bankAccount, bet)

