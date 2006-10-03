-- FairDie.hs
-- Problem 1.1

module Main where

outcomes, hits :: [Int]

outcomes = [1..6]
hits = [ x | x <- outcomes, x > 2 && x < 5 ]

prob :: Float
prob = (len hits) / (len outcomes)
    where len = fromIntegral . length

main :: IO ()
main = putStrLn $ "the probability is " ++ show prob ++ " (1/3 expected)"

{- __END__

A fair die is rolled in an honest manner. Determine the
probability that the number of spots that shows is more than
two but less than five.

-}
