-- BinSolve.hs
-- Evaluate the root of an equation using
--   binary division method
-- Copyright (c) 2005 Agent Zhang
-- 2005-09-11 2005-09-12

module BinSolve where

next a b = (b+a)/2

repeat' f a b x
    | (f a * f x < 0) = x : repeat' f a x (next a x)
    | otherwise       = x : repeat' f x b (next x b)

bsolve f a b cond
    | (f a) * (f b) < 0 && (a <= b) = Just (cond $ repeat' f a b (next a b))
    | otherwise                     = Nothing

within eps (a:a':as)
    | abs(a-a') <= eps = a'
    | otherwise        = within eps (a':as)

relative eps (a:a':as)
    | abs(a-a') <= eps * abs a' = a'
    | otherwise                 = relative eps (a':as)

g x = x^^3 - x^^2 - 2*x + 1

test1 = bsolve g 0 1 (within 0.0005)
test2 = bsolve g 0 1 (relative 0.0015)

h x = 2 * exp (-x) - (sin x)
test3 = bsolve h 0 1 (within 0.0005)
test4 = bsolve h 0 1 (relative 0.0015)
