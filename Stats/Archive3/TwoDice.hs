-- TwoDice.hs
-- Problems 1.2 and 1.3

module Main where

outcomes, hitsA, hitsB, hitsC :: [(Int, Int)]

outcomes = [ (x, y) | x <- [1..6], y <- [1..6] ]

hitsA = [ (x, y) | (x, y) <- outcomes, x + y == 7 ]
hitsB = [ (x, y) | (x, y) <- outcomes, even $ x + y ]
hitsC = [ (x, y) | (x, y) <- outcomes, x == y ]

prob :: [(Int, Int)] -> Float
prob hits = (len hits) / (len outcomes)
    where len = fromIntegral . length

main :: IO ()
main = do
    putStrLn $ "(a) " ++ (show $ prob hitsA) ++ " (1/6 expected)"
    putStrLn $ "(b) " ++ (show $ prob hitsB) ++ " (1/2 expected)"
    putStrLn $ "(c) " ++ (show $ prob hitsC) ++ " (1/6 expected)"

{- __END__

Consider two fair dice that are distinguishable; we
label them as die 1 and die 2. Let these dice be rolled
in an honest manner. Assume that all 6^2 = 36
possible outcome pairs have probability 1/36.
Determine the probability that the total number of
spots showing on the two dice (a) equals 7; (b)
is an even number. (c) Determine the probability that
the number of spots showing on the two dice coincide.

-}
