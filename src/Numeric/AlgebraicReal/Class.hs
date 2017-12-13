module Numeric.AlgebraicReal.Class where
import Data.Ratio
import qualified Data.Vector as V

infixl 7 `divide`
infixl 7 `modD`

-- | 整域
--
-- \(1 \ne 0\) となる可換環で、非自明な零因子を持たないものを整域という。
class (Num a) => IntegralDomain a where
  -- | 除算
  --
  -- @a@ が @b@ の倍数であるとき、 @divide a b@ は @a = b * c@ となる @c@ を返す。
  -- @a@ が @b@ の倍数でない時の挙動は規定しない。
  divide :: a -> a -> a

-- | GCD 整域
class (IntegralDomain a) => GCDDomain a where
  -- | 最大公約元を計算する。
  gcdD :: a -> a -> a

  -- | 'V.Vector' を係数とする多項式の内容を計算する。
  --
  -- 'Eq' クラスのインスタンスである場合は、短絡評価できる。
  contentV :: V.Vector a -> a
  contentV = V.foldr gcdD 0

-- | ユークリッド整域
class (GCDDomain a) => EuclideanDomain a where
  -- | ユークリッド除算
  divModD :: a -> a -> (a, a)
  modD :: a -> a -> a
  modD x y = snd (divModD x y)


instance (Integral a) => IntegralDomain (Ratio a) where
  divide = (/)

instance IntegralDomain Integer where
  divide = div

-- | 体の場合の gcdD 関数のデフォルト実装
fieldGcd :: (Eq a, Fractional a) => a -> a -> a
fieldGcd 0 0 = 0
fieldGcd _ _ = 1

-- | 体の場合の contentV 関数のデフォルト実装
fieldContentV :: (Eq a, Fractional a) => V.Vector a -> a
fieldContentV xs | V.null xs = 0
                 | otherwise = V.last xs

instance (Integral a) => GCDDomain (Ratio a) where
  gcdD = fieldGcd
  contentV = fieldContentV

instance GCDDomain Integer where
  gcdD = gcd
  contentV xs | V.null xs = 0
              | V.last xs < 0 = negate (gcdV 0 xs) -- 内容の符号に、最高次の係数の符号を反映させる
              | otherwise = gcdV 0 xs -- 短絡評価を考えなければ foldr gcd 0 でも良い
    where
      -- foldl/foldr と gcd の組み合わせでは GCD が 1 になっても残りの部分が評価される。
      -- 列の途中で GCD が 1 になれば全体の GCD は 1 で確定なので、そういう短絡評価する。
      gcdV 1 _ = 1
      gcdV a v | V.null v = a
               | otherwise = gcdV (gcdD (V.last v) a) (V.init v)

instance EuclideanDomain Integer where
  divModD = divMod

-- UniPoly に対する各インスタンスは UniPoly.hs で定義する
