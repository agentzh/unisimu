-- Tickets2.hs
-- Problem 1.5

module Main where

outcomes, hits :: [(Int, Int)]

outcomes = [ (x, y) | x <- [1..3], y <- [1..3] ]
hits = [ (x, y) | (x, y) <- outcomes, x /= 1 && y /= 1 ]

prob :: Float
prob = (len hits) / (len outcomes)
    where len = fromIntegral . length

main :: IO ()
main = putStrLn $ "the probability is " ++ show prob ++ " (4/9 expected)"

{- __END__

A box has three tickets, labeled from 1 to 3. A ticket
is selected at random from the box and then returned
to the box, after which a second ticket is selected
at random from the box. Assume that all 3^2 = 9
possible outcome pairs have probability 1/9. Determine
the probability that the ticket having label 1 is
not selected on either trial.

-}
