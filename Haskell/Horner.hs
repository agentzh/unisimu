-- Horner.hs
-- Evaluate the value of a polynormial using
--   Horner's Method («ÿæ≈…ÿ∑®)
-- Copyright (c) 2005 Agent Zhang
-- 2005-09-11 2005-09-12

module Horner where

-- Use the Horner method to calculate the value of polynomials:
next x b a = b * x + a
f aas x = foldl (next x) (head aas) (tail aas)

-- Antoher recursive version:
f' (a:a':as) x = f' ((next x a a'):as) x
f' [a] x = a

test1 = f [3,0,-1,1,2] 2
test2 = f' [3,0,-1,1,2] 2

test3 = f [8,-0.4,4,0,-9,1] 3
test4 = f' [8,-0.4,4,0,-9,1] 3
