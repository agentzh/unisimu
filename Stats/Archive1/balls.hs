-- balls.hs

module Main where

import Data.List

data Color = Red | Blue
    deriving (Show, Eq, Ord)

urn :: [Color]
urn = [Red, Red, Red, Blue]

outcomes, hits :: [(Color, Color)]

outcomes = [ (x, y) | x <- urn, y <- delete x urn ]
hits = [ (x, y) | (x, y) <- outcomes, x == Red && y == Red ]

prob :: Float
prob = (len hits) / (len outcomes)
    where len = fromIntegral . length

main :: IO ()
main = putStrLn $ "the probability is " ++ show prob ++
    " (1/2 expected)"

{- __END__

Example A

An urn contains three red balls and one blue ball. Two
balls are selected without replacement. What is the
probability that they are both red?

Let R1 and R2 denote the events that a red ball is drawn
on the first trial and on the second trial, respectively.
From the multiplication law,

    P(R1 กษ R2) = P(R1) P(R2 | R1)

P(R1) is clearly 3/4, and if a red ball has been removed
on the first trial, there are two red balls and one blue
ball left. Therefore, P(R2 | R1) = 2/3. Thus, P(R1 กษ R2)
= 1/2.

-}
