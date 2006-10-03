-- Tickets.hs
-- Problem 1.5

module Main where

import System.Environment
import System.Random

experiment :: Int -> IO [Int]
experiment count = sequence $ take count ls'
    where ls' = rollDie : ls'

rollDie :: IO Int
rollDie = randomRIO (1::Int, 100)

hits :: [Int] -> [Int]
hits outcomes = [ x | x <- outcomes, perfectSquare x ]

perfectSquare :: Int -> Bool
perfectSquare x = root^2 == x
    where root = round $ sqrt $ fromIntegral x

prob :: [Int] -> [Int] -> Float
prob hits outcomes = (len hits) / (len outcomes)
    where len = fromIntegral . length

main :: IO ()
main = do
    args <- getArgs
    let count = initCount args
    outcomes <- experiment count
    -- putStrLn $ show outcomes
    -- putStrLn $ show $ hits outcomes
    putStrLn $ "the probability is " ++
        (show $ prob (hits outcomes) outcomes) ++
        " (1/10 expected)"
    where
        initCount [] = 10000
        initCount (x:xs) = read x

{- __END__

A box has 100 tickets, labeld from 1 to 100. A ticket
is selected at random from the box. Determine the 
probability that the label of the ticket is a perfect
square (1, 4, 9, 16, ...).

-}
