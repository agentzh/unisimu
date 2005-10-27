-- SecSolve.hs
-- Evaluate the root of an equation using
--   Secant Method
-- Copyright (c) 2005 Agent Zhang
-- 2005-09-26 2005-09-26

module SecSolve where

next x x' f = x' - f x' * (x' - x) / (f x' - f x)

repeat' x x' f = x : repeat' x' (next x x' f) f

ssolve x0 x1 f cond = cond $ repeat' x0 x1 f

-- Starting of the usual part for error control --

within eps (a:a':as)
    | abs(a-a') <= eps = a'
    | otherwise        = within eps (a':as)

within' eps (a:a':as)
    | abs(a-a') <= eps = 1
    | otherwise        = 1 + within' eps (a':as)

relative eps (a:a':as)
    | abs(a-a') <= eps * abs a' = a'
    | otherwise                 = relative eps (a':as)

relative' eps (a:a':as)
    | abs(a-a') <= eps * abs a' = 1
    | otherwise                 = 1 + relative' eps (a':as)

-- End of the ususal part --

f x  = x*(x + 1)^2 - 1

test1   = ssolve 0.4 0.6 f (within  0.00005)
test1'  = ssolve 0.4 0.6 f (within' 0.00005)
test1'' = take 6 (repeat' 0.4 0.6 f)

g x  = x^3 - 2*x - 5

test2   = ssolve 2 2.2 g (within  0.0005)
test2'  = ssolve 2 2.2 g (within' 0.0005)
test2'' = take 5 (repeat' 2 2.2 g)

{-

-}