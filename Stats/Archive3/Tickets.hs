-- Tickets.hs
-- Problem 1.5

module Main where

outcomes, hits :: [Int]

outcomes = [1..100]
hits = [ x | x <- outcomes, perfectSquare x ]

perfectSquare :: Int -> Bool
perfectSquare x = root^2 == x
    where root = round $ sqrt $ fromIntegral x

prob :: Float
prob = (len hits) / (len outcomes)
    where len = fromIntegral . length

main :: IO ()
main = putStrLn $ "the probability is " ++ show prob ++ " (1/10 expected)"

{- __END__

A box has 100 tickets, labeld from 1 to 100. A ticket
is selected at random from the box. Determine the 
probability that the label of the ticket is a perfect
square (1, 4, 9, 16, ...).

-}
