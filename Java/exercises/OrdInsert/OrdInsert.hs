-- OrdInsert.hs

module Main where

import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    prog <- getProgName
    if length args > 0
        then processArgs $ map read args
        else help prog

processArgs :: [Int] -> IO ()
processArgs = putStr . unwords . map show . ordSort

ordSort :: Ord a => [a] -> [a]
ordSort = foldl ordInsert []

ordInsert :: Ord a => [a] -> a -> [a]
ordInsert ls x = left ++ [x] ++ right
    where (left, right) = span (<x) ls

help :: String -> IO ()
help prog = hPutStrLn stderr $ "Usage: " ++ prog ++ " <int>+"
