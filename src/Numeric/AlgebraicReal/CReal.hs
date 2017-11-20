module Numeric.AlgebraicReal.CReal where
import Numeric.AlgebraicReal.Interval
import Data.Maybe

newtype CReal = CReal [Interval]

instance Num CReal where
  negate (CReal xs)   = CReal (map negate xs)
  CReal xs + CReal ys = CReal (zipWith (+) xs ys)
  CReal xs - CReal ys = CReal (zipWith (-) xs ys)
  CReal xs * CReal ys = CReal (zipWith (*) xs ys)
  abs (CReal xs)      = CReal (map abs xs)
  signum (CReal xs)   = error "signum on CReal is not computable"
  fromInteger n       = CReal $ repeat (fromInteger n)

instance Fractional CReal where
  recip (CReal xs) = CReal $ map recip (dropWhile (\(Iv a b) -> a <= 0 && b <= 0) xs)
  fromRational x = CReal $ repeat (fromRational x)

unsafeCompareCReal :: CReal -> CReal -> Ordering
unsafeCompareCReal (CReal xs) (CReal ys) = head $ catMaybes (zipWith maybeCompareInterval xs ys)
