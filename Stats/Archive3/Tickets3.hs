-- Tickets3.hs
-- Problem 1.6

module Main where

import Data.List

outcomes, hits :: [(Int, Int)]

outcomes = [ (x, y) | x <- [1..3], y <- delete x [1..3] ]
hits = [ (x, y) | (x, y) <- outcomes, x /= 1 && y /= 1 ]

prob :: Float
prob = (len hits) / (len outcomes)
    where len = fromIntegral . length

main :: IO ()
main = putStrLn $ "the probability is " ++ show prob ++ " (1/3 expected)"

{- __END__

A box has three tickets, labeled from 1 to 3. A
ticket is selected at random from the box. Assume
that all 3 * 2 = 6 possible outcome pairs have
probability 1/6. Determine the probability that
the ticket having label 1 is not selected on
either trial.

-}
