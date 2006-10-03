-- balls.hs

module Main where

import Data.List

data Color = Red | Blue
    deriving (Show, Eq, Ord)

urn :: [Color]
urn = [Red, Red, Red, Blue]

outcomes, hits :: [(Color, Color)]

outcomes = [ (x, y) | x <- urn, y <- delete x urn ]
hits = [ (x, y) | (x, y) <- outcomes, y == Red ]

prob :: Float
prob = (len hits) / (len outcomes)
    where len = fromIntegral . length

main :: IO ()
main = putStrLn $ "the probability is " ++ show prob ++
    " (3/4 expected)"

{- __END__

Example C

Referring to Example A, what is the probability that a
red ball is selected on the second draw?

The answer may or may not be intuitively obvious--that
depends on your intuition. On the one hand, you could
argue that it is "clear from symmetry" that P(R2) =
P(R1) = 3/4. On the other hand, you could say that it
is obvious that a red ball is likely to be selected on
the first draw, leaving fewer red balls for the second
draw, so that P(R2) < P(R1). The answer can be derived
easily by using the law of total probability:

    P(R2) = P(R2 | R1)P(R1) + P(R2 | B1)P(B1)
          = 2/3 x 3/4 + 1 x 1/4 = 3/4

where B1 denotes the event that a blue ball is drawn
on the first trial.

-}
