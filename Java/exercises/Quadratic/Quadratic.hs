-- Quadratic.hs

module Main where

import System.Environment

type Equation = (Double, Double, Double)
type Solution = (Double, Double)

main :: IO ()
main = do 
    args <- getArgs
    prog <- getProgName
    if length args == 3
        then showSol $ toTuple args
        else printUsage prog

toTuple :: [String] -> (Double, Double, Double)
toTuple args = (read $ args !! 0, read $ args !! 1, read $ args !! 2)

showSol :: Equation -> IO ()
showSol equa = case solve equa of
    Left  err -> putStrLn $ "Error: " ++ err
    Right (x1, x2) -> putStrLn $ "Solution found: " ++ show x1 ++
        ", " ++ show x2

solve :: Equation -> Either String Solution
solve (a, b, c)
    | a == 0    = Left "It's not a quadratic equation"
    | delta < 0 = Left "No solution found."
    | otherwise = Right (solve' a b delta)
        where delta = b * b - 4 * a * c

solve' :: Double -> Double -> Double -> (Double, Double)
solve' a b delta = (p+q, p-q) where
    p = -b / (2*a)
    q = (sqrt delta) / (2*a)

printUsage :: String -> IO ()
printUsage s = fail $ "Usage: " ++ s ++ " <a> <b> <c>"
