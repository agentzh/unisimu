-- QuickSort.hs

module Main where

import System.Environment
import System.IO
import Data.List

main :: IO ()
main = do
    args <- getArgs
    prog <- getProgName
    if length args > 0
        then processArgs $ map read args
        else help prog

processArgs :: [Int] -> IO ()
processArgs = putStr . unwords . map show . quickSort

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = (quickSort left) ++ [x] ++ (quickSort right)
    where (left, right) = partition (x>=) xs

help :: String -> IO ()
help prog = hPutStrLn stderr $ "Usage: " ++ prog ++ " <int>+"
