{-# LANGUAGE BangPatterns #-}
module Numeric.AlgebraicReal.AlgReal where
import Numeric.AlgebraicReal.Class
import Numeric.AlgebraicReal.UniPoly
import Numeric.AlgebraicReal.Resultant
import Numeric.AlgebraicReal.Interval
import Numeric.AlgebraicReal.CReal
import qualified Data.Vector as V
import Data.List
import Data.Ratio

data ExtReal a = NegativeInfinity
               | Finite !a
               | PositiveInfinity
               deriving (Eq,Ord,Show)

clamp :: (Ord a) => a -> a -> ExtReal a -> a
clamp lb ub NegativeInfinity = lb
clamp lb ub (Finite x) | x < lb = lb
                       | ub < x = ub
                       | otherwise = x
clamp lb ub PositiveInfinity = ub

-- | 数の符号を 'Int' で返す
sign :: (Ord a, Num a) => a -> Int
sign x = case compare x 0 of
           EQ -> 0
           LT -> -1
           GT -> 1

-- | 指定した点における多項式の値の符号を返す
signAt :: (Ord a, Num a) => a -> UniPoly a -> Int
signAt x p = sign (valueAt x p)

signAtZQ :: Rational -> UniPoly Integer -> Int
signAtZQ x p = sign (valueAtZQ x p)

isZeroAtZQ :: Rational -> UniPoly Integer -> Bool
isZeroAtZQ x p = valueAtZQ x p == 0

-- | 指定した点における多項式の値の符号を返す（補完数直線版）
signAtX :: (Ord a, Num a) => ExtReal a -> UniPoly a -> Int
signAtX (Finite x) p = signAt x p
signAtX PositiveInfinity p
  | p == 0 = 0
  | otherwise = sign (leadingCoefficient p)
signAtX NegativeInfinity p
  | p == 0 = 0
  | otherwise = sign (leadingCoefficient p) * (-1)^(degree' p)

signAtZQX :: ExtReal Rational -> UniPoly Integer -> Int
signAtZQX (Finite x) p = signAtZQ x p
signAtZQX PositiveInfinity p
  | p == 0 = 0
  | otherwise = sign (leadingCoefficient p)
signAtZQX NegativeInfinity p
  | p == 0 = 0
  | otherwise = sign (leadingCoefficient p) * (-1)^(degree' p)

-- | Negative polynomial remainder sequence
negativePRS_f :: (Eq a, Fractional a) => UniPoly a -> UniPoly a -> [UniPoly a]
negativePRS_f f 0 = [f]
negativePRS_f f g = let r = f `modP` g in f : negativePRS_f g (-r)

-- | Negative polynomial remainder sequence using subresultant PRS
--
-- Assumption: 'degree f > degree g'
negativePRS :: (Ord a, IntegralDomain a) => UniPoly a -> UniPoly a -> [UniPoly a]
negativePRS f g = f : g : loop 1 f 1 g (subresultantPRS' f g)
  where
    loop !_ _ !_ _ [] = []
    loop !s f !t g ((b,x):xs)
      -- b * (lc g)^(degree f - degree g + 1) * s > 0
      | sign b * (sign $ leadingCoefficient g)^(degree' f - degree' g + 1) * s > 0 = -x : loop t g (-1) x xs
      | otherwise = x : loop t g 1 x xs

variance :: [Int] -> Int
variance = loop 0
  where
    loop :: Int -> [Int] -> Int
    loop !n [] = n
    loop !n [_] = n
    loop !n (x:xs@(y:ys))
      | x == 0 = loop n xs
      | y == 0 = loop n (x:ys)
      | x * y < 0 = loop (n + 1) xs
      | otherwise = loop n xs

varianceAt :: (Ord a, Num a) => a -> [UniPoly a] -> Int
varianceAt x ys = variance $ map (signAt x) ys

varianceAtX :: (Ord a, Num a) => ExtReal a -> [UniPoly a] -> Int
varianceAtX x ys = variance $ map (signAtX x) ys

varianceAtZQ :: Rational -> [UniPoly Integer] -> Int
varianceAtZQ x ys = variance $ map (signAtZQ x) ys

varianceAtZQX :: ExtReal Rational -> [UniPoly Integer] -> Int
varianceAtZQX x ys = variance $ map (signAtZQX x) ys

countRealRootsBetween :: (Ord a, Fractional a, IntegralDomain a) => a -> a -> UniPoly a -> Int
countRealRootsBetween a b f = varianceAt a s - varianceAt b s
  where s = negativePRS f (diffP f)

countRealRootsBetweenX :: (Ord a, Fractional a, IntegralDomain a) => ExtReal a -> ExtReal a -> UniPoly a -> Int
countRealRootsBetweenX a b f = varianceAtX a s - varianceAtX b s
  where s = negativePRS f (diffP f)

countRealRootsBetweenZQ :: Rational -> Rational -> UniPoly Integer -> Int
countRealRootsBetweenZQ a b f = varianceAtZQ a s - varianceAtZQ b s
  where s = negativePRS f (diffP f)

countRealRootsBetweenZQX :: ExtReal Rational -> ExtReal Rational -> UniPoly Integer -> Int
countRealRootsBetweenZQX a b f = varianceAtZQX a s - varianceAtZQX b s
  where s = negativePRS f (diffP f)

-- | 代数的実数を表す型
data AlgReal = FromRat !Rational
             | AlgReal !(UniPoly Integer) !Int !Rational !Rational
  deriving (Show)

-- | 定義多項式
definingPolynomial :: AlgReal -> UniPoly Integer
definingPolynomial (FromRat x) = fromCoeff (V.fromList [- numerator x, denominator x])
definingPolynomial (AlgReal p _ _ _) = p

-- | 実根の分離区間
isolatingInterval :: AlgReal -> (Rational, Rational)
isolatingInterval (FromRat x) = (x - 1, x + 1)
isolatingInterval (AlgReal _ _ x y) = (x, y)

-- s: この代数的実数における f' の符号（正なら区間 (a,b) において f は負から正に変わり、負なら区間 (a,b) において f は正から負に変わる）
intervalsWithSign :: UniPoly Integer -> Int -> Rational -> Rational -> [Interval]
intervalsWithSign !f !s !a !b = Iv a b : ivs a b
  where
    ivs !a !b | signAtZQ c f == 0 = repeat (Iv c c)
              | s * signAtZQ c f < 0 = Iv c b : ivs c b
              | s * signAtZQ c f > 0 = Iv a c : ivs a c
      where c = (a + b) / 2

-- | 近似する区間の列
intervals :: AlgReal -> [Interval]
intervals (FromRat x) = repeat (Iv x x)
intervals (AlgReal f s a b) = intervalsWithSign f s a b

-- | 与えられた定義多項式と、分離区間 (a,b] から、代数的実数を構築する。
mkAlgReal :: UniPoly Integer -> Rational -> Rational -> AlgReal
mkAlgReal f a b
  -- 0 の場合は FromRat 0 を使う
  | a < 0 && b >= 0 && valueAt 0 f == 0 = FromRat 0

  -- 区間が空の場合はエラー
  | b <= a = error "mkAlgReal: empty range"

  -- 区間の端点で多項式の値が 0 でないようにする
  | isZeroAtZQ b' f' = FromRat b'

  -- 定数項の 0 を取り除き、また、区間の符号が確定したものをデータ構築子として使う
  | otherwise = AlgReal f' s a' b'
  where nonZeroPart xs | V.head xs == 0 = nonZeroPart (V.tail xs)
                       | otherwise = xs
        f' = UniPoly $ nonZeroPart (coeff f)
        s | signAtZQ b f' > 0 = 1
          | signAtZQ b f' < 0 = -1
          | otherwise = signAtZQ b (diffP f')
        Just (Iv a' b') = find (\(Iv x y) -> 0 < x || y < 0) (intervalsWithSign f' s a b)

mkAlgRealWithInterval :: UniPoly Integer -> Interval -> AlgReal
mkAlgRealWithInterval f (Iv a b) = mkAlgReal f a b

algRealToCReal :: AlgReal -> CReal
algRealToCReal x = CReal (intervals x)

rootBound :: UniPoly Integer -> Rational
rootBound f | f == 0 = error "rootBound: polynomial is zero"
            | otherwise = 1 + (V.maximum $ V.map (abs . (% lc)) $ V.init $ coeff f)
  where lc = leadingCoefficient f

realRoots :: UniPoly Integer -> [AlgReal]
realRoots f = realRootsBetween f NegativeInfinity PositiveInfinity

realRootsBetween :: UniPoly Integer -> ExtReal Rational -> ExtReal Rational -> [AlgReal]
realRootsBetween f lb ub
  | f == 0 = error "realRoots: zero" -- 多項式 0 の実根を求めようとするのはエラー
  | degree' f == 0 = []                 -- 多項式が 0 でない定数の場合、実根はない
  | otherwise = bisect (lb',varianceAtZQX lb seq) (ub',varianceAtZQX ub seq)
  where
    f' = squareFree f               -- 無平方多項式に直す
    seq = negativePRS f' (diffP f') -- f' のスツルム列
    bound = rootBound f'            -- 根の限界
    lb' = clamp (-bound) bound lb   -- 与えられた区間の下界を有理数にしたもの
    ub' = clamp (-bound) bound ub   -- 与えられた区間の上界を有理数にしたもの

    -- 実装のキモ：与えられた区間の実根を列挙する。区間の端点におけるスツルム列の符号の変化も受け取る。
    bisect :: (Rational,Int) -> (Rational,Int) -> [AlgReal]
    bisect p@(a,i) q@(b,j)
      | i <= j     = []                       -- 区間に実根が存在しない場合
      | i == j + 1 = [mkAlgReal f a b]        -- 区間にちょうど一個の実根が存在する場合
      | otherwise  = bisect p r ++ bisect r q -- それ以外：複数個の実根が存在するので区間を分割する
      where c = (a + b) / 2
            r = (c,varianceAtZQ c seq)

realRootsBetweenQ :: UniPoly Rational -> ExtReal Rational -> ExtReal Rational -> [AlgReal]
realRootsBetweenQ f lb ub
  | f == 0 = error "realRoots: zero"
  | degree' f == 0 = []
  | otherwise = let commonDenominator = V.foldl' (\a b -> lcm a (denominator b)) 1 (coeff f)
                    fz = primitivePart $ mapCoeff (\x -> numerator x * (commonDenominator `div` denominator x)) f
                in realRootsBetween fz lb ub

instance Eq AlgReal where
  -- 有理数同士は普通に比較
  FromRat x == FromRat y = x == y

  -- 有理数と代数的実数。後者が有理数である可能性は排除されていないので、愚直にチェックする。
  FromRat x == y
    | x <= a || b <= x = False      -- 区間の中にない場合
    | otherwise = isZeroAtZQ x f    -- 定義多項式に代入して0になれば等しい
    where f = definingPolynomial y
          (a,b) = isolatingInterval y

  -- 同様
  x == FromRat y
    | y <= a || b <= y = False      -- 区間の中にない場合
    | otherwise = isZeroAtZQ y f    -- 定義多項式に代入して0になれば等しい
    where f = definingPolynomial x
          (a,b) = isolatingInterval x

  -- 代数的実数同士。持っている多項式が最小多項式とは限らないので、GCDを計算してそれが指定した区間に根を持っているか調べる。
  x == y
    | b  <= a' = False  -- 区間が重なっていない場合1
    | b' <= a  = False  -- 区間が重なっていない場合2
    | otherwise = countRealRootsBetweenZQ a'' b'' g == 1
    where f = definingPolynomial x
          (a,b) = isolatingInterval x
          f' = definingPolynomial y
          (a',b') = isolatingInterval y
          g = gcdD f f'
          a'' = max a a'
          b'' = min b b'

instance Ord AlgReal where
  -- 有理数同士の比較
  compare (FromRat x) (FromRat y) = compare x y

  -- 有理数と代数的実数の比較
  compare (FromRat x) y
    | x <= a = LT
    | b <= x = GT
    | countRealRootsBetweenZQ x b f == 1 = LT
    | isZeroAtZQ x f = EQ
    | otherwise = GT
    where f = definingPolynomial y
          (a,b) = isolatingInterval y

  -- 代数的実数と有理数の比較
  compare x (FromRat y)
    | y <= a = GT
    | b <= y = LT
    | countRealRootsBetweenZQ y b f == 1 = GT
    | isZeroAtZQ y f = EQ
    | otherwise = LT
    where f = definingPolynomial x
          (a,b) = isolatingInterval x

  -- 代数的実数同士の比較
  compare x y
    | b  <= a' = LT -- 区間が重なっていない場合1（y の方が大きい）
    | b' <= a  = GT -- 区間が重なっていない場合2（x の方が大きい）

    | countRealRootsBetweenZQ a'' b'' g == 1 = EQ -- 等しいかどうか？

    -- x と y が等しくないことが確定した場合、縮小する区間の列を使って比較する（計算可能実数みたいな感じ）
    | otherwise = unsafeCompareCReal (algRealToCReal x) (algRealToCReal y)

    where f = definingPolynomial x       -- x の定義多項式
          (a,b) = isolatingInterval x    -- x の区間
          f' = definingPolynomial y      -- y の定義多項式
          (a',b') = isolatingInterval y  -- y の区間
          g = gcdD f f'
          a'' = max a a'  -- x の区間と y の区間の共通部分の、下限
          b'' = min b b'  -- 同、上限

-- | 与えられた無平方多項式と、その根に収束する計算可能実数から、代数的実数を構築する。
mkAlgRealWithCReal :: UniPoly Integer -> CReal -> AlgReal
mkAlgRealWithCReal f (CReal xs) = mkAlgRealWithInterval f (head $ dropWhile (\(Iv a b) -> countRealRootsBetweenZQ a b f >= 2) xs)

instance Num AlgReal where
  negate (FromRat x) = FromRat (negate x)
  negate (AlgReal f s a b) = AlgReal (compP f (-ind)) (-s) (-b) (-a)

  FromRat x + FromRat y = FromRat (x + y)
  FromRat k + AlgReal f s a b = AlgReal (compP f (definingPolynomial (FromRat k))) s (a + k) (b + k)
  x@(AlgReal _ _ _ _) + y@(FromRat _) = y + x
  x + y = mkAlgRealWithCReal (squareFree $ resultant_poly f_x_y g) (algRealToCReal x + algRealToCReal y)
    where f = mapCoeff constP $ definingPolynomial x
          f_x_y = compP f (constP ind - ind) -- f(x-y)
          g = mapCoeff constP $ definingPolynomial y

  FromRat x - FromRat y = FromRat (x - y)
  FromRat k - AlgReal f s a b = AlgReal (compP f (definingPolynomial (FromRat k))) (-s) (k - b) (k - a)
  AlgReal f s a b - FromRat k = AlgReal (compP f (definingPolynomial (FromRat (-k)))) s (a - k) (b - k)
  x - y = mkAlgRealWithCReal (squareFree $ resultant_poly f_x_y g) (algRealToCReal x - algRealToCReal y)
    where f = mapCoeff constP $ definingPolynomial x
          f_x_y = compP f (constP ind + ind) -- f(x+y)
          g = mapCoeff constP $ definingPolynomial y

  FromRat x * FromRat y = FromRat (x * y)
  FromRat k * AlgReal f s a b
    | k == 0 = 0
    | k > 0 = AlgReal f_x_k s (a * k) (b * k)
    | k < 0 = AlgReal f_x_k (-s) (b * k) (a * k)
    where f_x_k = fst $ homogeneousValueAt (scaleP (denominator k) ind) (fromInteger $ numerator k) (mapCoeff fromInteger f) -- f(x/k)

  x@(AlgReal _ _ _ _) * y@(FromRat _) = y * x
  x * y = mkAlgRealWithCReal (squareFree $ resultant_poly y_f_x_y g) (algRealToCReal x * algRealToCReal y)
    where f = definingPolynomial x
          y_f_x_y = UniPoly $ V.reverse $ V.imap (\i a -> constP a * ind^i) $ coeff f -- y^n f(x/y)
          g = mapCoeff constP $ definingPolynomial y

  abs x | x >= 0 = x
        | otherwise = negate x

  signum x | x > 0 = 1
           | x == 0 = 0
           | x < 0 = -1

  fromInteger n = FromRat (fromInteger n)

instance Fractional AlgReal where
  recip (FromRat x) = FromRat (recip x)
  recip (AlgReal f s a b) = AlgReal (UniPoly $ V.reverse $ coeff f) s' (recip b) (recip a)
    where s' | even (degree' f) || 0 < a = -s
             | otherwise = s

  fromRational = FromRat

-- | Square root of a rational number
sqrtQ :: Rational -> AlgReal
sqrtQ a | a > 0 = case realRootsBetweenQ (ind^2 - constP a) (Finite 0) PositiveInfinity of
            [sqrt_a] -> sqrt_a
            _ -> error "sqrt: none or multiple roots"
        | a == 0 = 0
        | otherwise = error "sqrt: negative"

-- | Square root of an algebraic real number
sqrtA :: AlgReal -> AlgReal
sqrtA (FromRat x) = sqrtQ x
sqrtA x = case filter (\y -> FromRat a < y^2 && y^2 <= FromRat b) $ realRootsBetween (compP f (ind^2)) (Finite 0) PositiveInfinity of
                          [sqrtx] -> sqrtx
                          r -> error $ "sqrt: none or multiple roots" ++ show r
    where f = definingPolynomial x
          (a,b) = isolatingInterval x

-- | n-th root of a rational number
nthRootQ :: Int -> Rational -> AlgReal
nthRootQ !n !a
  | n == 0 = error "0th root"
  | n < 0  = nthRootQ (-n) (recip a)
  | a > 0  = case realRootsBetweenQ (ind^n - constP a) (Finite 0) PositiveInfinity of
               [b] -> b
               l -> error ("nthRoot: none or multiple roots " ++ show l)
  | a == 0 = 0
  | odd n  = case realRootsBetweenQ (ind^n - constP a) NegativeInfinity (Finite 0) of
               [b] -> b
               l -> error ("nthRoot: none or multiple roots " ++ show l)
  | otherwise = error "nthRoot: negative"

-- | n-th root of an algebraic real number
nthRootA :: Int -> AlgReal -> AlgReal
nthRootA !n (FromRat x) = nthRootQ n x
nthRootA !n x
  | n == 0 = error "0th root"
  | n < 0  = nthRootA (-n) (recip x)
  -- now n must be positive
  | x == 0 = 0
  | x > 0  = case filter (\x -> FromRat a < x^n && x^n <= FromRat b) $ realRootsBetween (compP f (ind^n)) (Finite 0) PositiveInfinity of
               [rx] -> rx
               _ -> error "nthRoot: none or multiple roots"
  -- x must be negative
  | odd n  = case filter (\x -> FromRat a < x^n && x^n <= FromRat b) $ realRootsBetween (compP f (ind^n)) NegativeInfinity (Finite 0) of
               [rx] -> rx
               _ -> error "nthRoot: none or multiple roots"
  | otherwise = error "nthRoot: negative"
  where f = definingPolynomial x
        (a,b) = isolatingInterval x

-- | Compute the power of an algebraic real number to an integer
powIntA :: AlgReal -> Int -> AlgReal
powIntA _ 0 = 1
powIntA x n | n < 0 = recip $ powIntA x (-n)
powIntA (FromRat x) n = FromRat (x^n)
powIntA x n = case (ind^n) `modP` f of
                g -> valueAt x (mapCoeff FromRat g) -- x^n mod f(x) (ind^n `modP` f)
  where f = mapCoeff fromInteger $ definingPolynomial x :: UniPoly Rational

powRatA :: AlgReal -> Rational -> AlgReal
powRatA x y = nthRootA (fromInteger $ denominator y) (powIntA x (fromInteger $ numerator y))

valueAtA :: AlgReal -> UniPoly Rational -> AlgReal
valueAtA (FromRat x) f = FromRat (valueAt x f)
valueAtA x f = case f `modP` g of
                 h -> valueAt x (mapCoeff FromRat h)
  where g = mapCoeff fromInteger $ definingPolynomial x :: UniPoly Rational
