-- 2005-10-12

module Guass where

import Data.List

type Matrix k = [Row k]          -- matrix represented by a list of its rows
type Row k    = [k]              -- a row represented by a list of literals

echelon   :: Matrix Double -> Matrix Double
echelon rs
    | null rs || null (head rs) = rs
    | null rs2                  = map (0:) (echelon (map tail rs))
    | otherwise                 = piv : map (0:) (echelon rs')
      where rs'            = map (adjust piv) (rs1++rs3)
            (rs1,rs2)      = span leadZero rs
            leadZero (n:_) = n==0
            (piv:rs3)      = rs2

adjust              :: Num a => Row a -> Row a -> Row a
adjust (m:ms) (n:ns) = zipWith (-) (map (n*) ms) (map (m*) ns)

---------------------------------------------------

echelon'   :: Matrix Double -> Matrix Double
echelon' rs
    | null rs || null (head rs) = rs
    | null rs2                  = map (0:) (echelon (map tail rs))
    | otherwise                 = piv : map (0:) (echelon rs')
      where rs'            = map (adjust' piv) (rs1++rs3)
            (rs1,rs2)      = span leadZero rs
            leadZero (n:_) = approxEq n 0 1e-15
            (piv:rs3)      = spanMaster rs2

adjust'              :: Row Double -> Row Double -> Row Double
adjust' (m:ms) (n:ns) = zipWith (-) ns (map (factor*) ms)
    where factor = n/m

spanMaster :: Matrix Double -> Matrix Double
spanMaster rs = piv:rs1++rs3
    where elem = maximum (map (abs.head) rs)
          (rs1,rs2) = span notMatch rs
          (piv:rs3) = rs2
          notMatch (n:_) = not $ approxEq n elem 1e-15

approxEq x y eps = abs (x-y) <= eps
