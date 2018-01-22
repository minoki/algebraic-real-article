module Numeric.AlgebraicReal.Factor.BigPrime where
import Prelude hiding (toInteger)
import Numeric.AlgebraicReal.UniPoly
import Numeric.AlgebraicReal.Factor.CantorZassenhaus (factorCZ)
import Data.FiniteField (PrimeField,char,toInteger)
import System.Random (RandomGen,getStdRandom)
import qualified Data.Vector as V
import Control.Monad (guard)
import Data.Proxy (Proxy)
import GHC.TypeLits (KnownNat)
import Data.Reflection (reifyNat)
import Math.NumberTheory.Powers.Squares (integerSquareRoot)
import Math.NumberTheory.Primes.Sieve (sieveFrom)
import Control.Arrow (first,second)

-- 1-norm of coefficients
oneNorm :: (Ord a, Num a) => UniPoly a -> a
oneNorm f = V.sum $ V.map abs $ coeff f

-- max-norm of coefficients
maxNorm :: (Ord a, Num a) => UniPoly a -> a
maxNorm f = V.maximum $ V.map abs $ coeff f

factorCoefficientBound :: UniPoly Integer -> Integer
factorCoefficientBound f = (integerSquareRoot (n + 1) + 1) * 2^n * maxNorm f -- 2^k * oneNorm f
  where n = fromIntegral (degree' f)

-- @partitions n s@ returns all possible partitions @(t,u)@ with @t ++ u == s@ (as a set) and @length t == n@
partitions :: Int -> [a] -> [([a],[a])]
partitions 0 [] = [([],[])]
partitions n [] = []
partitions n (x:xs) = map (first (x:)) (partitions (n-1) xs) ++ map (second (x:)) (partitions n xs)

primeFieldFromInteger :: (KnownNat p) => Proxy p -> Integer -> PrimeField p
primeFieldFromInteger _ = fromInteger

-- converts a @PrimeField p@ value into integer in (-p/2,p/2]
toInteger' :: (KnownNat p) => PrimeField p -> Integer
toInteger' x | x' * 2 <= p = x'
             | otherwise = x' - p
  where p = char x
        x' = toInteger x

-- Input: nonzero, primitive, squarefree
factorInt :: (RandomGen g) => UniPoly Integer -> g -> ([UniPoly Integer], g)
factorInt f =
  let bound = factorCoefficientBound f * leadingCoefficient f
      p:_ = filter (\p -> reifyNat p coprimeModP f (diffP f)) $ sieveFrom (2 * bound + 1)
  in reifyNat p factorWithPrime bound f

factorIntIO :: UniPoly Integer -> IO [UniPoly Integer]
factorIntIO f = getStdRandom (factorInt f)

coprimeModP :: (KnownNat p) => Proxy p -> UniPoly Integer -> UniPoly Integer -> Bool
coprimeModP proxy f g =
  let f' = mapCoeff (primeFieldFromInteger proxy) f
      g' = mapCoeff (primeFieldFromInteger proxy) g
  in degree (gcdP f' g') == Just 0

-- p must be prime
factorWithPrime :: (KnownNat p, RandomGen g) => Proxy p -> Integer -> UniPoly Integer -> g -> ([UniPoly Integer], g)
factorWithPrime proxy bound f gen =
  let f' = toMonic $ mapCoeff (primeFieldFromInteger proxy) f -- :: UniPoly (PrimeField p)
      (modularFactors, gen') = factorCZ f' gen
  in (factorCombinations bound 1 f modularFactors, gen')

factorCombinations :: (KnownNat p) => Integer -> Int -> UniPoly Integer -> [UniPoly (PrimeField p)] -> [UniPoly Integer]
factorCombinations bound k f [] = [] -- f must be 1
factorCombinations bound k f modularFactors
  | 2 * k > length modularFactors = [f] -- f is already irreducible
  | otherwise = case tryFactorCombinations of
      [] -> factorCombinations bound (k+1) f modularFactors
      (g,h,rest):_ -> g : factorCombinations bound k h rest
  where tryFactorCombinations = do
          (s,rest) <- partitions k modularFactors
          -- length s == k, s ++ rest == modularFactors (as a set)
          let lc_f = fromInteger (leadingCoefficient f)
              g = mapCoeff toInteger' (lc_f * product s)
              h = mapCoeff toInteger' (lc_f * product rest)
          guard (oneNorm g * oneNorm h <= bound)
          return (primitivePart g, primitivePart h, rest)
