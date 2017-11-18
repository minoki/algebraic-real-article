{-# LANGUAGE BangPatterns #-}
module Numeric.AlgebraicReal.Resultant where
import Numeric.AlgebraicReal.UniPoly

-- | 体係数多項式の終結式
resultant :: (Eq a, Fractional a) => UniPoly a -> UniPoly a -> a
resultant f g
  | (f == 0 && degree g == Just 0) || (degree f == Just 0 && g == 0) = 1
  | f == 0 || g == 0 = 0
  | degree' f == 0 = leadingCoefficient f ^ degree' g
  | otherwise = loop 1 f g
  where
    -- invariant: loop p f g = p * resultant f g, f /= 0, g /= 0
    loop p f g
      | degree' g == 0 = p * lc_g ^ degree' f
      | r == 0 = 0
      | otherwise = let !s = degree' g * degree' f
                        !j = degree' f - degree' r
                    in loop ((-1)^s * lc_g^j * p) g r
      where r = f `modP` g
            lc_g = leadingCoefficient g

-- Resultant.hs に追記
resultant_int :: UniPoly Integer -> UniPoly Integer -> Integer
resultant_int f g
  | (f == 0 && degree g == Just 0) || (degree f == Just 0 && g == 0) = 1
  | f == 0 || g == 0 = 0
  | degree' f == 0 = leadingCoefficient f ^ degree' g
  | degree' g == 0 = leadingCoefficient g ^ degree' f
  | r == 0 = 0
  | degree' f >= degree' g, l >= 0 = (-1)^(degree' f * degree' g) * lc_g^l * resultant_int g r
  | degree' f >= degree' g, l < 0  = (-1)^(degree' f * degree' g) * resultant_int g r `div` lc_g^(-l)
  | otherwise = (-1)^(degree' f * degree' g) * resultant_int g f
  where
    r = pseudoModP f g
    lc_g = leadingCoefficient g
    l = degree' f - degree' r - (degree' f - degree' g + 1) * degree' g

resultant_poly :: (Eq a, Fractional a) => UniPoly (UniPoly a) -> UniPoly (UniPoly a) -> UniPoly a
resultant_poly f g
  | (f == 0 && degree g == Just 0) || (degree f == Just 0 && g == 0) = 1
  | f == 0 || g == 0 = 0
  | degree' f == 0 = leadingCoefficient f ^ degree' g
  | degree' g == 0 = leadingCoefficient g ^ degree' f
  | r == 0 = 0
  | degree' f >= degree' g, l >= 0 = (-1)^(degree' f * degree' g) * lc_g^l * resultant_poly g r
  | degree' f >= degree' g, l < 0  = (-1)^(degree' f * degree' g) * resultant_poly g r `divP` lc_g^(-l)
  | otherwise = (-1)^(degree' f * degree' g) * resultant_poly g f
  where
    r = pseudoModP f g
    lc_g = leadingCoefficient g
    l = degree' f - degree' r - (degree' f - degree' g + 1) * degree' g

-- 整数係数多項式の、擬除算による剰余列を計算する
pseudoEuclidPRS :: (Eq a, Num a) => UniPoly a -> UniPoly a -> [UniPoly a]
pseudoEuclidPRS _ 0 = []
pseudoEuclidPRS f g = case pseudoModP f g of
  0 -> []
  rem -> rem : pseudoEuclidPRS g rem

-- 整数係数多項式の原始剰余列を計算する
primitivePRS_int :: UniPoly Integer -> UniPoly Integer -> [UniPoly Integer]
primitivePRS_int _ 0 = []
primitivePRS_int f g = case pseudoModP f g of
  0 -> []
  rem -> let !r' = primitivePart_int rem in r' : primitivePRS_int g r'

reducedPRS_int :: UniPoly Integer -> UniPoly Integer -> [UniPoly Integer]
reducedPRS_int _ 0 = []
reducedPRS_int f g = case pseudoModP f g of
  0 -> []
  rem -> rem : loop (degree' f) g rem
  where
    loop :: Int -> UniPoly Integer -> UniPoly Integer -> [UniPoly Integer]
    loop !deg_h f g = case pseudoModP f g of
      0 -> []
      rem -> let !deg_f = degree' f
                 !beta = leadingCoefficient f ^ (deg_h - deg_f + 1)
                 !mr = mapCoeff (`div` beta) rem
             in mr : loop deg_f g mr

subresultantPRS_int :: UniPoly Integer -> UniPoly Integer -> [UniPoly Integer]
subresultantPRS_int _ 0 = []
subresultantPRS_int f g = case pseudoModP f g of
  0 -> []
  rem -> let !d = degree' f - degree' g
             !s = (-1)^(d + 1) * rem
         in s : loop d (-1) g s
  where
    loop _ _ _ 0 = []
    loop d psi f g = case pseudoModP f g of
      0 -> []
      rem -> let !d' = degree' f - degree' g
                 !c = leadingCoefficient f
                 !psi' = (-c)^d `div` psi^(d-1)
                 !beta = -c * psi' ^ d'
                 !s = mapCoeff (`div` beta) rem
             in s : loop d' psi' g s

subresultantPRS_int' :: UniPoly Integer -> UniPoly Integer -> [(Integer,UniPoly Integer)]
subresultantPRS_int' _ 0 = []
subresultantPRS_int' f g = case pseudoModP f g of
  0 -> []
  rem -> let !d = degree' f - degree' g
             !s = (-1)^(d + 1) * rem
         in ((-1)^(d + 1), s) : loop d (-1) g s
  where
    loop _ _ _ 0 = []
    loop d psi f g = case pseudoModP f g of
      0 -> []
      rem -> let !d' = degree' f - degree' g
                 !c = leadingCoefficient f
                 !psi' = (-c)^d `div` psi^(d-1)
                 !beta = -c * psi' ^ d'
                 !s = mapCoeff (`div` beta) rem
             in (beta,s) : loop d' psi' g s
