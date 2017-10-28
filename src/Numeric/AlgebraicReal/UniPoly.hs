module Numeric.AlgebraicReal.UniPoly where
import qualified Data.Vector as V
import Data.Vector ((!))

-- 一変数多項式 (univariate polynomial)
newtype UniPoly a = UniPoly (V.Vector a)
  deriving (Eq,Show)

-- 多項式としてのゼロ
-- 末尾の P は polynomial の頭文字のつもり（以下同様）
zeroP :: UniPoly a
zeroP = UniPoly V.empty

-- 定数項のみの多項式
constP :: (Eq a, Num a) => a -> UniPoly a
constP 0 = zeroP
constP a = UniPoly (V.singleton a)

-- 不定元 (indeterminate)
ind :: (Num a) => UniPoly a
ind = UniPoly (V.fromList [0,1])

-- 多項式の係数を Vector として得る（昇冪の順）
coeff :: UniPoly a -> V.Vector a
coeff (UniPoly xs) = xs

-- 係数の列から多項式を作る
-- 具体的には、最高次の係数が 0 にならないようにリストの後ろの方を取り除く
fromCoeff :: (Eq a, Num a) => V.Vector a -> UniPoly a
fromCoeff xs
  | V.null xs      = zeroP
  | V.last xs == 0 = fromCoeff (V.init xs)
  | otherwise      = UniPoly xs

-- 多項式の次数
-- ゼロの場合は Nothing を返す。
-- （Maybe 型については Nothing < Just _ となるため、
-- 　順序関係に関しては Nothing を -∞ として扱うことができる）
degree :: UniPoly a -> Maybe Int
degree (UniPoly xs) = case V.length xs - 1 of
  -1 -> Nothing
  n -> Just n

-- 多項式の次数
-- ゼロの場合はエラーとする。
degree' :: UniPoly a -> Int
degree' (UniPoly xs) = case V.length xs of
  0 -> error "degree': zero polynomial"
  n -> n - 1

-- 最高次の係数
leadingCoefficient :: (Num a) => UniPoly a -> a
leadingCoefficient (UniPoly xs)
  | V.null xs = 0
  | otherwise = V.last xs

-- モニック多項式への変換：係数を最高次の係数で割る
toMonic :: (Fractional a) => UniPoly a -> UniPoly a
toMonic f@(UniPoly xs)
  | V.null xs = zeroP
  | otherwise = UniPoly $ V.map (* recip (leadingCoefficient f)) xs

instance (Eq a, Num a) => Num (UniPoly a) where
  negate (UniPoly xs) = UniPoly $ V.map negate xs

  UniPoly xs + UniPoly ys
    | n < m = UniPoly $ V.accumulate (+) ys (V.indexed xs)
    | m < n = UniPoly $ V.accumulate (+) xs (V.indexed ys)
    | n == m = fromCoeff $ V.zipWith (+) xs ys
    where n = V.length xs
          m = V.length ys

  -- multiplication: naive method
  UniPoly xs * UniPoly ys
    | n == 0 || m == 0 = zeroP
    | otherwise = UniPoly $ V.generate (n + m - 1) (\i -> sum [(xs ! j) * (ys ! (i - j)) | j <- [0..i], j < n, i - j < m])
    where n = V.length xs
          m = V.length ys

  fromInteger n = constP $ fromInteger n

  -- these should be kicked out of 'Num' class...
  abs = error "abs of a polynomial is nonsense"
  signum = error "signum of a polynomial is nonsense"

-- scalar multiplication
scaleP :: (Eq a, Num a) => a -> UniPoly a -> UniPoly a
scaleP a (UniPoly xs)
  | a == 0 = zeroP
  | otherwise = UniPoly $ V.map (* a) xs

valueAt :: (Num a) => a -> UniPoly a -> a
valueAt t (UniPoly xs) = V.foldr' (\a b -> a + t * b) 0 xs

-- 'f `compP` g = f(g(x))'
compP :: (Eq a, Num a) => UniPoly a -> UniPoly a -> UniPoly a
compP (UniPoly xs) g = V.foldr' (\a b -> constP a + g * b) 0 xs

divModP :: (Eq a, Fractional a) => UniPoly a -> UniPoly a -> (UniPoly a, UniPoly a)
divModP f g
  | g == 0    = error "divModP: divide by zero"
  | degree f < degree g = (zeroP, f)
  | otherwise = loop zeroP (scaleP (recip b) f)
  where
    g' = toMonic g
    b = leadingCoefficient g
    -- invariant: f == q * g + scaleP b p
    loop q p | degree p < degree g = (q, scaleP b p)
             | otherwise = let q' = UniPoly (V.drop (degree' g) (coeff p))
                           in loop (q + q') (p - q' * g')

divP f g = fst (divModP f g)
modP f g = snd (divModP f g)

gcdP :: (Eq a, Fractional a) => UniPoly a -> UniPoly a -> UniPoly a
gcdP f g | g == 0    = f
         | otherwise = gcdP g (f `modP` g)

-- 余りを計算するごとにモニック多項式に変換する
monicGcdP :: (Eq a, Fractional a) => UniPoly a -> UniPoly a -> UniPoly a
monicGcdP f g | g == 0    = f
              | otherwise = monicGcdP g (toMonic (f `modP` g))

diffP :: (Eq a, Num a) => UniPoly a -> UniPoly a
diffP (UniPoly xs)
  | null xs = zeroP
  | otherwise = fromCoeff $ V.tail $ V.imap (\i x -> fromIntegral i * x) xs

squareFree :: (Eq a, Fractional a) => UniPoly a -> UniPoly a
squareFree f = f `divP` gcdP f (diffP f)
