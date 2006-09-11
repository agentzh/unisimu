module Main where
import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    prog <- getProgName
    if length args == 1 then printRes $ args !! 0 else printUsage prog

printRes :: String -> IO ()
printRes s = putStrLn $ show $ (5::Float) / 9 * (val - 32) where val = read s

printUsage :: String -> IO ()
printUsage prog = hPutStrLn stderr $ "Usage: " ++ prog ++ " <num>"
