module Numeric.AlgebraicReal.Interval where

-- Iv a b represents the interval [a,b]
data Interval = Iv !Rational !Rational deriving (Show)

width :: Interval -> Rational
width (Iv a b) = b - a

instance Num Interval where
  negate (Iv a b) = Iv (-b) (-a)
  Iv a b + Iv c d = Iv (a + c) (b + d)
  Iv a b - Iv c d = Iv (a - d) (b - c)
  Iv a b * Iv c d = Iv (minimum [a*c,a*d,b*c,b*d]) (maximum [a*c,a*d,b*c,b*d])
  abs x@(Iv a b) | 0 <= a = x
                 | b <= 0 = -x
                 | otherwise = Iv 0 (max (-a) b)
  signum (Iv a b) | 0 < a            = 1         -- 正の場合
                  | b < 0            = -1        -- 負の場合
                  | a == 0 && b == 0 = 0         -- 0 の場合
                  | 0 <= a           = Iv 0 1    -- 非負の場合
                  | b <= 0           = Iv (-1) 0 -- 非正の場合
                  | otherwise        = Iv (-1) 1 -- 符号不明の場合
  fromInteger n = Iv (fromInteger n) (fromInteger n)

instance Fractional Interval where
  recip (Iv a b) | 0 < a || b < 0 = Iv (recip b) (recip a)
                 | otherwise = error "divide by zero"
  fromRational x = Iv x x

maybeCompareInterval :: Interval -> Interval -> Maybe Ordering
maybeCompareInterval (Iv a b) (Iv c d)
  | b < c = Just LT
  | d < a = Just GT
  | a == d && b == c = Just EQ
  -- b <= c = LE
  -- d <= a = GE
  | otherwise = Nothing
