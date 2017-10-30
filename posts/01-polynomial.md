---
title: "#1 一変数多項式環"
date: 2017-10-14
---

# 一変数多項式環

代数的数を扱う上では、当然、多項式を操作することが必要になる。
#0 では整数係数、あるいは有理数係数の多項式に言及したが、追い追い、多項式環自身や、有限体などを係数とする多項式環を扱うことになる。
そこで、多項式環の係数環は、一般の整域 \(R\) とする。

整域 \(R\) 上の一変数多項式環 (univariate polynomial) \(R[x]\) は、不定元 \(x\) について \(a_n x^n+\cdots+a_1 x+a_0 \quad (a_i \in R)\) の形で表される元全体である。

\(R[x]\) の演算としては、まず和、差、積の環演算が挙げられる。それから、 \(R\) の元による乗算（スカラー倍）や、多項式除算などもある。
今後使うものをリストアップしてみよう：

* 環演算：和、差、積
* スカラー倍
* 多項式除算（余りつきの除算）
* 値の計算（\(x\) に \(R\) の元を代入する）
* 最大公約元 (GCD)
* 微分
* （多項式の因数分解もいずれ必要になるが、次回以降に回す）

## 多項式の表し方

多項式の表し方であるが、素朴に、係数の列として表すことにする。
つまり、\(n\) 次の多項式の場合は長さ \(n+1\) の列を使い、定数項を0番目、最高次の係数 (leading coefficient) を \(n\) 番目に格納する。
多項式がゼロの場合は長さ0の列を使う。

係数の列を表すのには、普通のプログラミング言語だと配列を使うところだろう。
配列を直接触っても良いし、配列をラップした型を用意しても良い。

Haskell には標準でリスト型 `[a]` と Data.Array モジュールの `Array` 型があるが、前者はランダムアクセスが苦手、後者は多次元も扱えるように一般化されすぎていて１次元では却って扱いづらい、という欠点がある。
そのため、ここでは [vector パッケージ](https://hackage.haskell.org/package/vector)の [Data.Vector](https://hackage.haskell.org/package/vector/docs/Data-Vector.html) モジュールで提供される `Vector` 型を使うことにする。
`Vector` 型に対しては、リスト同様の使い勝手と、 `Array` 同様の性能を期待できる。

```haskell
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
```

## 和、差、積、スカラー倍
和、差、積は素朴に筆算の通りに実装する。

```haskell
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
```

スカラー倍も、リストのそれぞれの要素を \(a\) 倍するだけだから簡単だろう。
```haskell
-- scalar multiplication
scaleP :: (Eq a, Num a) => a -> UniPoly a -> UniPoly a
scaleP a (UniPoly xs)
  | a == 0 = zeroP
  | otherwise = UniPoly $ V.map (* a) xs
```

## 値の計算
多項式の不定元に \(R\) の元 \(t\) を代入したものを、定義式の通りに
\[f(t)=a_n\cdot t^n+a_{n-1}\cdot t^{n-1}+\cdots+a_1\cdot t+a_0\]
と計算するのは効率的ではない。
**ホーナー法** (Horner's method) を使うと、乗算の回数を削減できる：
\[f(t)=(\cdots(a_n\cdot t+a_{n-1})\cdot t+\cdots)\cdot t+a_0\]

Haskell では `foldr` 関数を使うと簡単にホーナー法を実装できる：

```haskell
valueAt :: (Num a) => a -> UniPoly a -> a
valueAt t (UniPoly xs) = V.foldr' (\a b -> a + t * b) 0 xs
```

ここでは多項式を係数の列として表す際に定数項が先頭、最高次の係数が末尾としているので `foldr` を使ったが、逆に定数項が末尾、最高次が先頭の場合は `foldl` となる。

## 合成
多項式 \(f\) に別の多項式 \(g\) を合成する演算も、同様に実装する。
```haskell
-- 'f `compP` g = f(g(x))'
compP :: (Eq a, Num a) => UniPoly a -> UniPoly a -> UniPoly a
compP (UniPoly xs) g = V.foldr' (\a b -> constP a + g * b) 0 xs
```

## 除算
除算も、筆算のアルゴリズムで適当に実装する。

```haskell
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
```

整数と多項式で、余りつきの除算を統一的に扱えると嬉しいが、 Haskell 標準の型クラスではそのようにできない。

## 最大公約元
最大公約元 (GCD; greatest common divisor) の計算は、整数と同じ**ユークリッドの互除法** (Euclidean algorithm) が使える。

```haskell
gcdP :: (Eq a, Fractional a) => UniPoly a -> UniPoly a -> UniPoly a
gcdP f g | g == 0    = f
         | otherwise = gcdP g (f `modP` g)
```

ただし、計算の途中に係数が複雑な有理数になる場合がある。
例として、\(f(x)=x^4+3x^3-2x+4\) と \(g(x)=x^3-7x^2+x-1\) の GCD を計算してみよう：

```haskell
-- 擬似 Haskell コード：
f = ind^4 + 3 * ind^3 - 2 * ind + 4 :: UniPoly Rational
g = ind^3 - 7 * ind^2 + ind - 1 :: UniPoly Rational
-- …のとき、
gcdP f g = gcdP g (69 * ind^2 - 11 * ind + 14)
         = gcdP (69 * ind^2 - 11 * ind + 14) (-1397/4761 * ind + 1847/4761)
         = gcdP (-1397/4761 * ind + 1847/4761) (234326898/1951609)
         = gcdP (234326898/1951609) 0
         = 234326898/1951609
```

最初に与えた多項式は整数係数の大人しい子であったにも関わらず、途中に出てくる多項式、そして最後の結果は、分母も分子も巨大な有理数となってしまった。
このような現象を**係数膨張** (coefficient growth) という。

係数膨張を抜本的に解決する方法として有限体を使うものがあるようだが、準備が大変そうなので今は扱わない。
ここでは、除算のたびに余りをモニック多項式に変換するという変種を提示するに留める。

```haskell
-- 余りを計算するごとにモニック多項式に変換する
monicGcdP :: (Eq a, Fractional a) => UniPoly a -> UniPoly a -> UniPoly a
monicGcdP f g | g == 0    = f
              | otherwise = monicGcdP g (toMonic (f `modP` g))
```

## 微分
多項式の（形式的な）微分は、定数項を取り除いて前に詰め、 \(n\) 次の項の係数だったものに \(n\) を掛ければ良い。

通常の微分は実数や複素数上の関数に対して行うが、形式微分は一般の環に対して定義できる。
なお、一般の環では、微分した際に次数が2以上下がる場合があるので注意されたい。

```haskell
diffP :: (Eq a, Num a) => UniPoly a -> UniPoly a
diffP (UniPoly xs)
  | null xs = zeroP
  | otherwise = fromCoeff $ V.tail $ V.imap (\i x -> fromIntegral i * x) xs
```

## 無平方成分
多項式の**無平方成分**の計算も、ここで実装しておく。

多項式 \(f(x)\) が異なる既約多項式の積として \(f(x)=f_1(x)^{l_1}\dots f_m(x)^{l_m}\) と因数分解されるとしよう。
この時、指数 \(l_i\) を全て1に変えたもの \(f_1(x)\dots f_m(x)\) を \(f\) の**無平方成分** (square-free part) という。
また、すでに指数 \(l_i\) が全て1であれば \(f\) は**無平方** (square-free) であるという。

多項式の無平方成分の計算であるが、微分とGCDの計算を組み合わせればすぐである：
\[\mathrm{squareFree}(f)=f/\mathrm{gcd}(f, f')\]
原理は簡単なので、自分で考えて欲しい。

```haskell
squareFree :: (Eq a, Fractional a) => UniPoly a -> UniPoly a
squareFree f = f `divP` gcdP f (diffP f)
```
