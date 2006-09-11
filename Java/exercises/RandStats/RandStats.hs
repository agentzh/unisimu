-- RandStats.hs

module Main where
import System.Random

main :: IO ()
main = do
    list <- genSeq
    putStrLn $ "Maximum is " ++ (show $ maximum list)
    putStrLn $ "Minimum is " ++ (show $ minimum list)
    putStrLn $ "Count of nums greater than 60 is " ++ (show $ nbig list)
    where
        nbig ls = length $ filter (60<) ls

genSeq :: IO [Float]
genSeq = do
    sequence $ take 100 genSeq'
    where
        genSeq' = randomRIO (0::Float, 100) : genSeq'
