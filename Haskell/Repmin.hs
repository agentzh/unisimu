{-
    Repmin stands for "replacing the integer valued leaves
    of a tree by the minimal integer value found in the
    leaves.
-}

module Main where

data Tree = TreeLeaf Int
          | TreeBin Tree Tree
          deriving Show

repmin :: Tree -> Tree
repmin t = t'
    where (t', tmin) = r t tmin

r :: Tree -> Int -> (Tree, Int)
r (TreeLeaf i) m = (TreeLeaf m, i)
r (TreeBin lt rt) m = (TreeBin lt' rt', lmin `min` rmin)
    where (lt', lmin) = r lt m
          (rt', rmin) = r rt m

tr = TreeBin (TreeLeaf 3) (TreeBin (TreeLeaf 4) (TreeLeaf 5))
tr' = repmin tr

main :: IO ()
main = putStrLn $ show tr'
