-- Compare3.hs

module Main where

import System.Environment
import System.IO
import Data.List

main :: IO ()
main = do
    args <- getArgs
    prog <- getProgName
    if length args == 3 then printRes args else printUsage prog

printRes :: [String] -> IO ()
printRes list = do
    putStrLn $ unwords sortedList
    where
        sortedList = sortBy (flip compare) list

printUsage :: String -> IO ()
printUsage s = hPutStrLn stderr $ "Usage: " ++ s ++ " <num> <num> <num>"
