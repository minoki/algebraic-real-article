module Numeric.AlgebraicReal.Factor.CantorZassenhaus where
import Prelude hiding (toInteger)
import Data.FiniteField
import System.Random
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.State
import GHC.TypeLits (KnownNat)
import Numeric.AlgebraicReal.UniPoly

powMod :: (Eq k, Fractional k) => UniPoly k -> Integer -> UniPoly k -> UniPoly k
powMod _ 0 _ = 1
powMod a n m = loop a (n-1) a
  where loop a 0 acc = acc
        loop a n acc = case n `quotRem` 2 of
          (n',0) -> loop (a * a `modP` m) n' acc
          (n',1) -> loop (a * a `modP` m) n' (acc * a `modP` m)

-- Input: nonzero, monic, squarefree
distinctDegreeFactorization :: (FiniteField k, Eq k) => UniPoly k -> [(Int,UniPoly k)]
distinctDegreeFactorization f = loop 1 ind f
  where loop k u g | degree' g == 0 = []
                   | degree' g < 2*k = [(degree' g,g)]
                   | f' == 1 = loop (k+1) u' g'
                   | otherwise = (k,f') : loop (k+1) u' g'
          where u' = powMod u q f
                f' = toMonic $ gcdP g (u' - ind)
                g' = g `divP` f'
        q = order (leadingCoefficient f)

randomPolyOfDegreeLessThan :: (Eq k, Num k, Random k, RandomGen g) => Int -> g -> (UniPoly k, g)
randomPolyOfDegreeLessThan n = runState $ fmap (fromCoeff . V.fromList) $ sequence $ replicate n $ state random

-- Input: nonzero, monic, squarefree, equal-degree, reducible
equalDegreeFactorizationOne :: (FiniteField k, Eq k, Random k, RandomGen g) => Int -> UniPoly k -> g -> (Maybe (UniPoly k), g)
equalDegreeFactorizationOne d h g
  = let (u,g') = randomPolyOfDegreeLessThan (degree' h) g
        m = do
          guard (u /= 0)
          let v = toMonic $ gcdP h u
          if v /= 1
            then return v
            else do
            let w = powMod u ((q^d-1) `div` 2) h
                h' = toMonic $ gcdP h (w-1)
            guard (h' /= 1 && h' /= h)
            return h'
    in (m,g')
  where q = order (leadingCoefficient h)

-- Input: nonzero, monic, squarefree, equal-degree
equalDegreeFactorization :: (FiniteField k, Eq k, Random k, RandomGen g) => Int -> UniPoly k -> g -> ([UniPoly k], g)
equalDegreeFactorization d h = runState (loop h [])
  where loop h acc | degree' h == 0 = return acc
                   | degree' h == d = return (h:acc)
                   | otherwise = do
                       m <- state (equalDegreeFactorizationOne d h)
                       case m of
                         Nothing -> loop h acc
                         Just h' -> do -- h' is a nontrivial factor of h
                           acc' <- loop h' acc
                           loop (h `divP` h') acc'

equalDegreeFactorizationIO :: (FiniteField k, Eq k, Random k) => Int -> UniPoly k -> IO [UniPoly k]
equalDegreeFactorizationIO d h = getStdRandom (equalDegreeFactorization d h)

-- Input: nonzero, monic, squarefree
factorCZ :: (FiniteField k, Eq k, Random k, RandomGen g) => UniPoly k -> g -> ([UniPoly k], g)
factorCZ f = runState
             $ fmap concat
             $ forM (distinctDegreeFactorization f)
             $ \(d,h) -> state (equalDegreeFactorization d h)

factorCZIO :: (FiniteField k, Eq k, Random k) => UniPoly k -> IO [UniPoly k]
factorCZIO f = getStdRandom (factorCZ f)

instance KnownNat p => Random (PrimeField p) where
  randomR (lo,hi) g = case randomR (toInteger lo, toInteger hi) g of
                        (a,g') -> (fromInteger a, g')
  random = randomR (minBound, maxBound)
