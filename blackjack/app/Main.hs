module Main where

import Lib

data Card = 
    Two | Three | Four | Five | 
    Six | Seven | Eight | Nine | 
    Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Enum)

main :: IO ()
main = someFunc
