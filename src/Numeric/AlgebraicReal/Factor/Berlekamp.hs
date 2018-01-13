module Numeric.AlgebraicReal.Factor.Berlekamp where
import Prelude hiding (toInteger)
import Data.FiniteField
import System.Random
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.State
import Data.Array
import Data.List
import Numeric.AlgebraicReal.UniPoly
import Numeric.AlgebraicReal.Factor.CantorZassenhaus (powMod)

type Mat a = Array (Int,Int) a

-- beta : x \mapsto x^q-x の表現行列を計算する。
betaMat :: (FiniteField k, Eq k) => UniPoly k -> Mat k
betaMat f
  = let q = order (leadingCoefficient f)
        n = degree' f
        xq = powMod ind q f -- x^q mod f
        ys = iterate (\y -> (y * xq) `modP` f) 1
    in array ((0,0),(n-1,n-1))
       [ ((i,j), mij - d)
       | (j,xqj) <- zip [0..n-1] ys
       , (i,mij) <- zip [0..n-1] (V.toList (coeff xqj) ++ repeat 0)
       , let d | i == j = 1
               | i /= j = 0
       ]

-- 列基本変形を行う
-- 非 0 な列の index と、変形後の行列を返す
elim :: (Fractional k, Eq k) => Int -> Mat k -> ([Int],Mat k)
elim imax m = loop [] i0 m
  where
    b@((i0,j0),(_,jn)) = bounds m
    loop seen i m
      | i > imax = (seen,m)
      | otherwise = case [k | k <- [j0..jn] \\ seen, m!(i,k) /= 0] of
                      [] -> loop seen (i+1) m
                      k:_ -> loop (k:seen) (i+1) $ array b
                             [ ((i',j),v)
                             | (i',j) <- indices m
                             , let v | j /= k = m!(i',j) - m!(i',k)*m!(i,j)/m!(i,k)
                                     | j == k = m!(i',k)
                             ]

-- Input: nonzero, monic, squarefree
berlekampBasis :: (FiniteField k, Eq k) => UniPoly k -> [UniPoly k]
berlekampBasis f
  = let n = degree' f
        m = betaMat f
        -- m' は m の拡大係数行列（m の下に単位行列をくっつけたもの）
        m' = array ((0,0),(2*n-1,n-1)) (assocs m ++ [ ((i+n,j),v)
                                                    | (i,j) <- indices m
                                                    , let v | i == j = 1
                                                            | i /= j = 0
                                                    ])
        (ix,l) = elim (n-1) m'
    in [fromCoeff (V.fromList [l!(n+i,j) | i<-[0..n-1]]) | j <- [0..n-1] \\ ix]

-- Input: nonzero, monic, squarefree
-- Returns Nothing if f is already irreducible
berlekampOne :: (FiniteField k, Eq k, Random k, RandomGen g) => UniPoly k -> g -> (Maybe (UniPoly k), g)
berlekampOne f = let bs = berlekampBasis f
                     r = length bs
                     q = order (leadingCoefficient f)

                     -- challenge :: State g (UniPoly k)
                     -- f の非自明な因数を見つけて返す
                     challenge = do
                       cs <- sequence $ replicate r $ state random -- F_q の元をランダムに r 個選ぶ
                       let a = sum (zipWith scaleP cs bs) -- 線形結合を作る
                           w = powMod a ((q-1) `div` 2) f -- a^((q-1)/2) mod f の計算
                           f' = toMonic $ gcdP f (w-1)
                       if f' /= 1 && f' /= f
                         then return f' -- f' は f の非自明な因数である
                         else challenge -- f' は非自明な因数ではなかった；係数を選ぶところからやり直し

                 in runState (if r == 1
                               then return Nothing -- f is already irreducible
                               else fmap Just challenge)

berlekampOneIO :: (FiniteField k, Eq k, Random k) => UniPoly k -> IO (Maybe (UniPoly k))
berlekampOneIO = getStdRandom . berlekampOne

-- Input: nonzero, monic, squarefree
factorBerlekamp :: (FiniteField k, Eq k, Random k, RandomGen g) => UniPoly k -> g -> ([UniPoly k], g)
factorBerlekamp f = runState (loop f [])
  where loop f acc | degree' f == 0 = return acc
                   | otherwise = do
                       mf' <- state (berlekampOne f)
                       case mf' of
                         Nothing -> return (f:acc) -- f is already irreducible
                         Just f' -> do
                           -- f' is a nontrivial factor of f
                           acc' <- loop f' acc
                           loop (f `divP` f') acc'

factorBerlekampIO :: (FiniteField k, Eq k, Random k) => UniPoly k -> IO [UniPoly k]
factorBerlekampIO = getStdRandom . factorBerlekamp
