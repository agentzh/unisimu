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
printRes = putStrLn . unwords . reverse . sort

printUsage :: String -> IO ()
printUsage s = fail $ "Usage: " ++ s ++ " <num> <num> <num>"
