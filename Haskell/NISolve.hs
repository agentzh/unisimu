-- NISolve.hs
-- Evaluate the root of an equation using
--   Newton's Iteration Method
-- Copyright (c) 2005 Agent Zhang
-- 2005-09-26 2005-09-26

module NISolve where

next x f f' = x - f x / f' x

repeat' x f f' = x : repeat' (next x f f') f f'

nsolve x0 f f' cond = cond $ repeat' x0 f f'

-- Starting of the usual part for error control --

within eps (a:a':as)
    | abs(a-a') <= eps = a'
    | otherwise        = within eps (a':as)

within' eps (a:a':as)
    | abs(a-a') <= eps = 1
    | otherwise        = 1 + within' eps (a':as)

-- End of the ususal part --

f x  = x*(x+1)^2 - 1
f' x = (x + 1)*(3*x + 1)

test1   = nsolve 0.4 f f' (within  0.00005)
test1'  = nsolve 0.4 f f' (within' 0.00005)
test1'' = take 4 (repeat' 0.4 f f')

g x  = x^2 - 135.607
g' x = 2*x

test2   = nsolve 12 g g' (within  0.000005)
test2'  = nsolve 12 g g' (within' 0.000005)
test2'' = take 4 (repeat' 12 g g')

h  x = x^5 - 235.4
h' x = 5 * x^4

test3   = nsolve 4 h h' (within  0.0005)
test3'  = nsolve 4 h h' (within' 0.0005)
test3'' = take 6 (repeat' 4 h h')

test4  x0 = nsolve x0 h h' (within  0.0005)
test4' x0 = nsolve x0 h h' (within' 0.0005)

{-

-}
