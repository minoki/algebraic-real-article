---
title: "#2 実根の数え上げ"
date: 2017-10-21
---
\newcommand\Integer{\mathbb{Z}}
\newcommand\Rational{\mathbb{Q}}
\newcommand\Real{\mathbb{R}}
\newcommand\GCD{\mathrm{gcd}}
\newcommand\sign{\mathrm{sign}}
\newcommand\abs[1]{\left\lvert{#1}\right\rvert}

多項式について、指定した区間にいくつ根があるか決定できる操作があると便利である。
これができると、多項式の実根の存在範囲をより精密化したり、（根の存在範囲がわかっていれば）多項式の実根を全て数え上げることができる。
そのために使えるのが、スツルムの定理である。

# スツルムの定理

多項式 \(f\in\Rational[x]\) と有理数の区間 \((a,b]\) が与えられた時、その区間における \(f\) の実根の個数を決定したい。
簡単のため、多項式 \(f\in\Real[x]\) は無平方であるとする（微分とGCDを使えば多項式を無平方にできることは、前回触れた）。

\(f\) の**スツルム列** (Sturm sequence, またはスツルム鎖 Sturm chain) \(\{f_0,\ldots,f_{l}\}\) を次のように定める：

- \(f_0=f\)
- \(f_1=f'\)
- \(1\le i\) について \(f_{i-1}=q_i f_i-f_{i+1},\ \deg f_{i+1}<\deg f_i\) （つまり、 \(f_{i+1}\) は \(f_{i-1}\) の \(f_i\) による剰余の符号を逆転させたもの）
- \(f_{l+1}=0\)

スツルム列について、次の性質が成り立つ（各自で確かめて欲しい）：

- \(f_i\) と \(f_{i+1}\) は互いに素である。特に、 \(f_l\) は定数で、符号が一定である。
- \(f_i\) は重根を持たない。
- 実数 \(a\) について \(f_i(a)=0\) であれば、 \(f_{i-1}(a)f_{i+1}(a)<0\) である。

実数列 \(\{a_0,\ldots,a_l\}\) の**符号の変化の数**とは、

i. 列から 0 を取り除いた時の、文字通りの符号の変化の数（0 を取り除いて得られた列を \(\{\tilde{a}_0,\ldots,\tilde{a}_m\}\) とした時の、 \(\tilde{a}_i \tilde{a}_{i+1}<0\) となる \(i\) の個数）、もしくは
ii. \(a_i>0,a_{i+1}\le 0\) または \(a_i<0,a_{i+1}\ge 0\) となる \(i\) の個数

のことである。
今回扱う列は、列の途中で 0 となる場合 (\(a_{i}=0\)) は必ずその前後で符号が変化する (\(a_{i-1}a_{i+1}<0\)) という性質を持つので、「0 を挟んで同符号となる」場合は考えなくて良い（つまり、 i. ii. どちらの定義を採用しても同じ）。

<div class="theorem">
**定理** (Sturm's theorem).
多項式 \(f\in\Real[x]\) は無平方であるとし、 \(f\) のスツルム列を \(\{f_0,\ldots,f_l\}\) とする。
実数 \(t\) について自然数 \(w(t)\) を、実数列 \(\{f_0(t),\ldots,f_l(t)\}\) の符号の変化の数と定める。
\(w(t)\) は単調減少であり、実数 \(b<c\) について \(w(b)-w(c)\) は 半開区間 \((b,c]\) における \(f\) の実根の個数を与える。
</div>
<div class="proof">
証明．
\(w\) が変化しうるのは、ある \(i\) について \(f_i(a)=0\) となるような \(a\) においてである。
そこで、 \(f_i(a)=0\) となるような任意の \(i\) と \(a\in\Real\) について、

- \(i=0\) ならば \(a\) の前後で \(w\) が減少する（\(\{f_0(t),f_1(t)\}\) の符号の変化の数が \(t<a\) と \(a\le t\) で異なる）
- \(i>0\) ならば、 \(f_i\) に起因する \(w\) の変化は起こらない（同じ \(a\) に関して \(f_0(a)=0\) となって \(w\) が変化する可能性は否定しない）

ことを示す。

\(i=0\) の場合。
\(f\) は重根を持たないので \(f'(a)>0\) または \(f'(a)<0\) のいずれかである。
それぞれの場合について、 \(f\) と \(f'\) の \(a\) の近傍における符号を表にしてみよう。
ただし、 \(\varepsilon\) は十分小さい正の数とする。

\(f'(a)>0\) の場合：

   \(t\)    \(a-\varepsilon\)   \(a\)   \(a+\varepsilon\)
---------- ------------------- ------- -------------------
 \(f(t)\)      &minus;            0        +
\(f'(t)\)         +               +        +

\(f'(a)<0\) の場合：

   \(t\)    \(a-\varepsilon\)    \(a\)    \(a+\varepsilon\)
---------- ------------------- --------- -------------------
 \(f(t)\)         +                0          &minus;
\(f'(t)\)      &minus;          &minus;       &minus;

いずれの場合も、部分列 \(\{f_0(t),f_1(t)\}\) の符号の変化の数は、 \(t<a\) と \(t\ge a\) で 1 から 0 と減少することが表からわかる。

\(i>0\) の場合。
\(f_i\) と \(f_{i-1}\) は互いに素なので、 \(f_{i-1}(a)\ne 0\) である。
\(a\) の前後で \(f_{i-1}\) と \(f_{i+1}\) の符号は変化しないこと、そして \(f_{i-1}(a)\) と \(f_{i+1}(a)\) の符号は逆であることに注意する。

\(f_{i-1}(a)>0\) と \(f_{i-1}(a)<0\) のそれぞれの場合について、 \(f_{i-1}\), \(f_i\) 及び \(f_{i+1}\) の \(a\) の近傍における符号を表にしてみよう。
ただし、 ? には、それぞれ任意の符号が入る。

\(f_{i-1}(a)>0\) の場合：

   \(t\)         \(a-\varepsilon\)    \(a\)    \(a+\varepsilon\)
--------------- ------------------- --------- -------------------
\(f_{i-1}(t)\)         +                +            +
  \(f_i(t)\)           ?                0            ?
\(f_{i+1}(t)\)      &minus;          &minus;      &minus;

\(f_{i-1}(a)<0\) の場合：

   \(t\)         \(a-\varepsilon\)    \(a\)    \(a+\varepsilon\)
--------------- ------------------- --------- -------------------
\(f_{i-1}(t)\)      &minus;          &minus;      &minus;
  \(f_i(t)\)           ?                0            ?
\(f_{i+1}(t)\)         +                +            +

いずれの場合も、部分列 \(\{f_{i-1}(t),f_i(t),f_{i+1}(t)\}\) の符号の変化の数は \(t=a\) の近傍で 1 のまま変化しないことが表からわかる。

以上が証明の要点である。端点 \(b,c\) の扱いなど、細かい部分は自分で詰めていただきたい。
証明終わり。
</div>

なお、スツルム列の要素 \(f_i\) は正の実数倍しても定理の適用に支障はない。
特に、係数膨張を抑えるために、剰余の計算の際に（\(\pm1\) 倍を除いた）モニック多項式を利用しても構わない。

また、スツルム列は符号が一定な多項式が出現した時点で打ち切って良い（\(x^2+1\) など）。

例として、実際の多項式についてのスツルム列と、その符号の変化を表にしてみた：
\[
\begin{aligned}
f(x)=f_0(x)&=x^4-2x^2+3x+1, \\
f'(x)=f_1(x)&=4x^3-4x+3, \\
f_2(x)&=x^2-\frac{9}{4}x-1, \\
f_3(x)&=-x-\frac{16}{27}, \\
f_4(x)&=-1.
\end{aligned}
\]
（ただし、 \(f_2\) 以降は、符号を除いたモニック多項式に直している）

  \(x\)    \(-\infty...\)   \(-1.83...\)     ...     \(-1.26...\)     ...    \(-0.59...\)     ...     \(-0.38...\)    ...     \(-0.28...\)     ...     \(2.63..\)   \(...\infty\)
--------- ---------------- -------------- --------- -------------- -------- -------------- --------- -------------- -------- -------------- --------- ------------ ---------------
 \(f\)           +              0          &minus;    &minus;       &minus;     &minus;     &minus;      &minus;     &minus;       0            +          +             +
 \(f_1\)       &minus;        &minus;      &minus;      0              +          +            +            +           +          +            +          +             +
 \(f_2\)         +              +            +          +              +          +            +            0        &minus;     &minus;     &minus;       0             +
 \(f_3\)         +              +            +          +              +          0         &minus;      &minus;     &minus;     &minus;     &minus;     &minus;      &minus;
 \(f_4\)       &minus;        &minus;      &minus;    &minus;       &minus;     &minus;     &minus;      &minus;     &minus;     &minus;     &minus;     &minus;      &minus;
符号の変化        3               2            2          2              2          2            2            2           2           1           1          1             1

# 根の限界

有限の区間における根の個数を求めるアルゴリズムがあるのはわかった。
あとは、実根の上界と下界を求めれば、多項式のすべての実根を決定できる。
ここでは、実根の上界と下界を与えるための簡単な方法を紹介する。

<div class="theorem">
**定理**．
多項式 \(f(x)=a_nx^n+a_{n-1}x^{n-1}+\cdots+a_1x+a_0\in\Real[x]\) に対し、
\[M=\max\left\{\frac{\abs{a_{n-1}}}{\abs{a_n}},\ldots,\frac{\abs{a_1}}{\abs{a_n}},\frac{\abs{a_0}}{\abs{a_n}}\right\}\]
とおく。
この時、 \(f\) の根の絶対値は \(M+1\) 未満である。
</div>
<div class="proof">
証明．
\(\abs{t}\ge M+1\) の時に \(f(t)\) が0でないことを示す。
\[\begin{aligned}
\abs{\frac{a_{n-1}}{a_n}t^{n-1}+\cdots+\frac{a_1}{a_n}t+\frac{a_0}{a_n}}
&\le \abs{\frac{a_{n-1}}{a_n}}\cdot\abs{t}^{n-1}+\cdots+\abs{\frac{a_1}{a_n}}\cdot\abs{t}+\abs{\frac{a_0}{a_n}} \\
&\le M(\abs{t}^{n-1}+\cdots+\abs{t}+1) \\
&\le M\frac{\abs{t}^n-1}{\abs{t}-1} \\
&\le \abs{t}^n-1 \\
\end{aligned}\]
より、
\[f(t)=a_n\left(t^n+\frac{a_{n-1}}{a_n}t^{n-1}+\cdots+\frac{a_1}{a_n}t+\frac{a_0}{a_n}\right)\ne 0\]
である。
</div>

## 正負の無限大における符号の変化

\(t\to{\pm\infty}\) における多項式の符号は、次数 \(n\) と最高次の係数 \(a_n\) を使って
\[\sign(f(t))=\begin{cases}
\sign(a_n) & (t\to{+\infty}) \\
({-1})^n \sign(a_n) & (t\to{-\infty})
\end{cases}\]
と表現できる。

特に、根の限界における符号を計算する際に、多項式に値を代入する必要はない。

# 実根の数え上げの実装

モジュール名は `Numeric.AlgebraicReal.AlgReal` とする。
前回作った `UniPoly` モジュールを import しておく。
```haskell
{-# LANGUAGE BangPatterns #-}
module Numeric.AlgebraicReal.AlgReal where
import Numeric.AlgebraicReal.UniPoly
import qualified Data.Vector as V
import Data.List
```

## 補完数直線

多項式について「実数 \(t\) または \(\pm\infty\) での符号を求める関数」を書きたいので、**補完数直線** (extended real number) を扱う型を作っておく（TODO: もっと良い訳語？）。
```haskell
data ExtReal a = NegativeInfinity
               | Finite !a
               | PositiveInfinity
               deriving (Eq,Ord,Show)
```
`Ord` クラスの導出インスタンスに関しては、 `NegativeInfinity < Finite _ < PositiveInfinity` が成り立つので、無限大を含めた順序関係は想定通りとなる。

例えば、 `ExtReal Rational` は有理数に \(\pm\infty\) を加えた集合を表す型となる。

補完数直線の元 \(x\) に対して、それを有限の範囲 \([l,u]\) に切り詰める、つまり \(\max\{\min\{x,u\},l\}\) みたいなことをする関数を用意しておく。
```haskell
clamp :: (Ord a) => a -> a -> ExtReal a -> a
clamp lb ub NegativeInfinity = lb
clamp lb ub (Finite x) | x < lb = lb
                       | ub < x = ub
                       | otherwise = x
clamp lb ub PositiveInfinity = ub
```

## 多項式の符号

多項式の符号を求める関数を作っておく。
正負の無限大における符号は、最高次の係数により計算する。
```haskell
-- | 数の符号を 'Int' で返す
sign :: (Ord a, Num a) => a -> Int
sign x = case compare x 0 of
           EQ -> 0
           LT -> -1
           GT -> 1

-- | 指定した点における多項式の値の符号を返す
signAt :: (Ord a, Num a) => a -> UniPoly a -> Int
signAt x p = sign (valueAt x p)

-- | 指定した点における多項式の値の符号を返す（補完数直線版）
signAtX :: (Ord a, Num a) => ExtReal a -> UniPoly a -> Int
signAtX (Finite x) p = signAt x p
signAtX PositiveInfinity p
  | p == 0 = 0
  | otherwise = sign (leadingCoefficient p)
signAtX NegativeInfinity p
  | p == 0 = 0
  | otherwise = sign (leadingCoefficient p) * (-1)^(degree' p)
```

## スツルム列と符号の変化の数

負係数多項式剰余列を計算する関数を作っておく。
```haskell
-- | Negative polynomial remainder sequence
negativePRS :: (Eq a, Fractional a) => UniPoly a -> UniPoly a -> [UniPoly a]
negativePRS f 0 = [f]
negativePRS f g = let r = f `modP` g in f : negativePRS g (-r)
```

符号 (`Int` で表す) の列に対し、符号の変化の数を返す関数を用意しておく。
```haskell
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
```

与えられたスツルム列の、指定した点における変化の数を求める関数：
```haskell
varianceAt :: (Ord a, Num a) => a -> [UniPoly a] -> Int
varianceAt x ys = variance $ map (signAt x) ys

varianceAtX :: (Ord a, Num a) => ExtReal a -> [UniPoly a] -> Int
varianceAtX x ys = variance $ map (signAtX x) ys
```

与えられた多項式 \(f\) について、指定した区間 \((a,b]\) における実根の数を数える関数：
```haskell
countRealRootsBetween :: (Ord a, Fractional a) => a -> a -> UniPoly a -> Int
countRealRootsBetween a b f = varianceAt a s - varianceAt b s
  where s = negativePRS f (diffP f)

countRealRootsBetweenX :: (Ord a, Fractional a) => ExtReal a -> ExtReal a -> UniPoly a -> Int
countRealRootsBetweenX a b f = varianceAtX a s - varianceAtX b s
  where s = negativePRS f (diffP f)
```
区間の端点として `a` を取るものと `ExtReal a` を取るものがあってだるい。

多項式 \(f\) に関するスツルム列と、その多項式の実根をただ一つ含むような区間 \((a,b]\) が与えられた時、その実根の有理数の区間による近似列を返す関数を作っておく。
```haskell
intervalsWithSturmSeq :: (Ord a, Fractional a) => [UniPoly a] -> a -> a -> [(a,a)]
intervalsWithSturmSeq seq a b = (a,b) : ivs a b
  where
    i = varianceAt a seq  -- 点 a での符号の変化の数
    ivs a b | i == j = (c,b) : ivs c b
            | i /= j = (a,c) : ivs a c
      where c = (a + b) / 2
            j = varianceAt c seq
```

# 代数的実数の実装

代数的実数を表す型も、ここで定義してしまおう。
```haskell
-- | 代数的実数を表す型
data AlgReal = FromRat !Rational
             | AlgReal !(UniPoly Rational) !Rational !Rational
  deriving (Show)

-- | 定義多項式
definingPolynomial :: AlgReal -> UniPoly Rational
definingPolynomial (FromRat x) = ind - constP x
definingPolynomial (AlgReal p _ _) = p

-- | 実根の分離区間
isolatingInterval :: AlgReal -> (Rational, Rational)
isolatingInterval (FromRat x) = (x - 1, x + 1)
isolatingInterval (AlgReal _ x y) = (x, y)

-- | 定義多項式のスツルム列
sturmSeq :: AlgReal -> [UniPoly Rational]
sturmSeq x = negativePRS f (diffP f)
  where f = definingPolynomial x

-- | 近似する区間の列
intervals :: AlgReal -> [(Rational,Rational)]
intervals (FromRat x) = repeat (x,x)
intervals x@(AlgReal _ a b) = intervalsWithSturmSeq (sturmSeq x) a b
```

ここでの代数的実数は

- 有理数（`FromRat` データ構築子）または
- **定義多項式** (defining polynomial; ここだけの定義で、「代数的数についてそれを根に持つような多項式」としておく) および分離区間の上端と下端（`AlgReal` データ構築子）

で表される。
今後、データ型の定義を変えるかもしれないので、データ型の定義に基づいたパターンマッチさせる箇所は最小限にして、 `definingPolynomial`, `isolatingInterval`, `sturmSeq` の各関数を通して構成要素にアクセスすることにする。

多項式の因数分解をまだ実装していないため、有理数であっても `AlgReal` データ構築子で表されている場合があるし、定義多項式が既約でない可能性もある。
因数分解を実装するまでの間は、代数的実数に関する各種操作はこれらを念頭に置いて実装する。

ただし、今後の都合で、代数的実数の符号はすぐに判別できるようにしておきたいし、定義多項式の定数項は非0としておきたい。
また、分離区間が半開区間だとややこしくなるので、区間の端点での多項式の値が 0 でないようにしておきたい。
つまり、 `AlgReal` データ構築子で値を作る際に、

- 定義多項式の定数項は 0 とならないこと、特に、 0 を表すのには `FromRat 0` のみを使うこと、
- 実根の区間の符号は正か負のいずれかが確定していること、
- 区間の端点での多項式の値は 0 でないこと

を要請する。
この要請を守りやすくするため、定義多項式と区間から代数的実数を構築するのには、 `AlgReal` ではなくそれをラップした `mkAlgReal` 関数を使う：
```haskell
-- | 与えられた定義多項式と、分離区間 (a,b] から、代数的実数を構築する。
mkAlgReal :: UniPoly Rational -> Rational -> Rational -> AlgReal
mkAlgReal f a b
  -- 0 の場合は FromRat 0 を使う
  | a < 0 && b >= 0 && valueAt 0 f == 0 = FromRat 0

  -- 区間が空の場合はエラー
  | b <= a = error "mkAlgReal: empty range"

  -- 区間の端点で多項式の値が 0 でないようにする
  | valueAt b' f' == 0 = FromRat b'

  -- 定数項の 0 を取り除き、また、区間の符号が確定したものをデータ構築子として使う
  | otherwise = AlgReal f' a' b'
  where nonZeroPart xs | V.head xs == 0 = nonZeroPart (V.tail xs)
                       | otherwise = xs
        f' = UniPoly $ nonZeroPart (coeff f)
        seq = negativePRS f' (diffP f')
        Just (a',b') = find (\(x,y) -> 0 < x || y < 0) (intervalsWithSturmSeq seq a b)
```

## 多項式の実根の列挙

実根の限界を求める関数を用意しておく：
```haskell
rootBound :: (Ord a, Fractional a) => UniPoly a -> a
rootBound f | f == 0 = error "rootBound: polynomial is zero"
            | otherwise = 1 + (V.maximum $ V.map (abs . (/ lc)) $ V.init $ coeff f)
  where lc = leadingCoefficient f
```

与えられた多項式の実根を列挙する（代数的実数のリストとして返す）操作は、次のように書ける：
```haskell
realRoots :: UniPoly Rational -> [AlgReal]
realRoots f = realRootsBetween f NegativeInfinity PositiveInfinity

realRootsBetween :: UniPoly Rational -> ExtReal Rational -> ExtReal Rational -> [AlgReal]
realRootsBetween f lb ub
  | f == 0 = error "realRoots: zero" -- 多項式 0 の実根を求めようとするのはエラー
  | degree' f == 0 = []                 -- 多項式が 0 でない定数の場合、実根はない
  | otherwise = bisect (lb',varianceAtX lb seq) (ub',varianceAtX ub seq)
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
            r = (c,varianceAt c seq)
```

## 代数的実数の比較演算

ここまでの知識で、代数的実数の比較演算も書ける。
まずは等号比較をやってみよう。

この段階では、 `AlgReal` データ構築子で表すものが有理数である可能性が排除されておらず、また、最小多項式の計算もできていない（定義多項式が既約とは限らない）ので、処理が多少冗長になる。
```haskell
instance Eq AlgReal where
  -- 有理数同士は普通に比較
  FromRat x == FromRat y = x == y

  -- 有理数と代数的実数。後者が有理数である可能性は排除されていないので、愚直にチェックする。
  FromRat x == y
    | x <= a || b <= x = False      -- 区間の中にない場合
    | otherwise = valueAt x f == 0  -- 定義多項式に代入して0になれば等しい
    where f = definingPolynomial y
          (a,b) = isolatingInterval y

  -- 同様
  x == FromRat y
    | y <= a || b <= y = False      -- 区間の中にない場合
    | otherwise = valueAt y f == 0  -- 定義多項式に代入して0になれば等しい
    where f = definingPolynomial x
          (a,b) = isolatingInterval x

  -- 代数的実数同士。持っている多項式が最小多項式とは限らないので、GCDを計算してそれが指定した区間に根を持っているか調べる。
  x == y
    | b  <= a' = False  -- 区間が重なっていない場合1
    | b' <= a  = False  -- 区間が重なっていない場合2
    | otherwise = countRealRootsBetween a'' b'' g == 1
    where f = definingPolynomial x
          (a,b) = isolatingInterval x
          f' = definingPolynomial y
          (a',b') = isolatingInterval y
          g = gcdP f f'
          a'' = max a a'
          b'' = min b b'
```

大小の比較も実装できる。
```haskell
instance Ord AlgReal where
  -- 有理数同士の比較
  compare (FromRat x) (FromRat y) = compare x y

  -- 有理数と代数的実数の比較
  compare (FromRat x) y
    | x <= a = LT
    | b <= x = GT
    | countRealRootsBetween x b f == 1 = LT
    | valueAt x f == 0 = EQ
    | otherwise = GT
    where f = definingPolynomial y
          (a,b) = isolatingInterval y

  -- 代数的実数と有理数の比較
  compare x (FromRat y)
    | y <= a = GT
    | b <= y = LT
    | countRealRootsBetween y b f == 1 = GT
    | valueAt y f == 0 = EQ
    | otherwise = LT
    where f = definingPolynomial x
          (a,b) = isolatingInterval x

  -- 代数的実数同士の比較
  compare x y
    | b  <= a' = LT -- 区間が重なっていない場合1（y の方が大きい）
    | b' <= a  = GT -- 区間が重なっていない場合2（x の方が大きい）

    | countRealRootsBetween a'' b'' g == 1 = EQ -- 等しいかどうか？

    -- x と y が等しくないことが確定した場合、縮小する区間の列を使って比較する（計算可能実数みたいな感じ）
    | otherwise = compareIntervals (intervals x) (intervals y)

    where f = definingPolynomial x       -- x の定義多項式
          (a,b) = isolatingInterval x    -- x の区間
          f' = definingPolynomial y      -- y の定義多項式
          (a',b') = isolatingInterval y  -- y の区間
          g = gcdP f f'
          a'' = max a a'  -- x の区間と y の区間の共通部分の、下限
          b'' = min b b'  -- 同、上限

          -- 縮小する区間の列が与えられた時、それらの大小を比較する（収束先が異なることが前提）
          compareIntervals :: (Ord a) => [(a,a)] -> [(a,a)] -> Ordering
          compareIntervals ((a,b):xs) ((a',b'):ys)
            | b <= a' = LT
            | b' <= a = GT
            | otherwise = compareIntervals xs ys
```

比較演算以外の操作、例えば四則演算などは、次回以降に実装する。

# 実行例

今回実装したものを、早速動かしてみよう。

\#0 で書いたように `stack new` されていると仮定し、
前回のソースコードは `src/Numeric/AlgebraicReal/UniPoly.hs`, 今回のソースコードは `src/Numeric/AlgebraicReal/AlgReal.hs` に保存する。
`algebraic-real.cabal` の以下の部分を
```
library
  hs-source-dirs:      src
  exposed-modules:     Lib
  build-depends:       base >= 4.7 && < 5
  default-language:    Haskell2010
```
次のように書き換える：
```
library
  hs-source-dirs:      src
  exposed-modules:     Numeric.AlgebraicReal.UniPoly
                     , Numeric.AlgebraicReal.AlgReal
  build-depends:       base >= 4.7 && < 5
                     , vector
  default-language:    Haskell2010
```

よくわからなかったら、**Stackに触れている**（つまり、最新の）Haskell入門書も参照してほしい（無責任！）。

セットアップとコードのコピペが終わったら
```
$ stack repl
```
を叩こう。
ソースコードのビルドが終わったら、次のようなプロンプトが現れるはずだ：
```
*Numeric.AlgebraicReal.UniPoly Numeric.AlgebraicReal.AlgReal Numeric.AlgebraicReal.UniPoly> 
```
以降の実行例で、プロンプトのモジュール部分は省略する。

## スツルム列の計算
まずはスツルム列を計算させてみよう。
先に書いた多項式 \(x^4-2x^2+3x+1\) を入力してみる。
`UniPoly` モジュールを書いた時、不定元は `ind` という名前にしたので、 \(x\) の代わりに `ind` を使う：
```
> let f = ind^4 - 2 * ind^2 + 3 * ind + 1
```
微分を計算してみる：
```
> let f' = diffP f
> f'
UniPoly [3,-4,0,4]
```
前回実装した多項式の表し方を思い出すと、 `UniPoly [3,-4,0,4]` は多項式 \(4x^3-4x+3\) を表すことがわかる。うまく計算できている。

負係数多項式剰余列を計算してみる：
```
> negativePRS f f'
[UniPoly [1.0,3.0,-2.0,0.0,1.0],UniPoly [3.0,-4.0,0.0,4.0],UniPoly [-1.0,-2.25,1.0],UniPoly [-12.0,-20.25],UniPoly [-0.6844993141289436]]
```
Haskellのお節介機能により、係数が Double になってしまった。
気を取り直して、有理数係数で計算してみる：
```
> negativePRS f f' :: [UniPoly Rational]
[UniPoly [1 % 1,3 % 1,(-2) % 1,0 % 1,1 % 1],UniPoly [3 % 1,(-4) % 1,0 % 1,4 % 1],UniPoly [(-1) % 1,(-9) % 4,1 % 1],UniPoly [(-12) % 1,(-81) % 4],UniPoly [(-499) % 729]]
```
わかりやすく数学風に書いてみると、
\[
\begin{aligned}
f_0(x)&=x^4-2x^2+3x+1, \\
f_1(x)&=4x^3-4x+3, \\
f_2(x)&=x^2-\frac{9}{4}x-1, \\
f_3(x)&=-\frac{81}{4}x-12, \\
f_4(x)&=-\frac{499}{729}.
\end{aligned}
\]
となる。

演習問題：\(g(x)=x^5+4x^3-x^2+3\) のスツルム列を計算せよ。
係数膨張を感じられただろうか？

演習問題：係数膨張を軽減した負係数多項式剰余列を計算する関数 `negativePRS'` を実装せよ。
具体的には、多項式の剰余の計算の際、剰余をその最高次の係数の絶対値で割り、符号を除いたモニック多項式に変換せよ。

## 実根の列挙

さっき REPL 上で定義した多項式 f の実根を列挙してみよう：
```
> realRoots f
[AlgReal (UniPoly [1 % 1,3 % 1,(-2) % 1,0 % 1,1 % 1]) ((-2) % 1) ((-1) % 1),AlgReal (UniPoly [1 % 1,3 % 1,(-2) % 1,0 % 1,1 % 1]) ((-1) % 2) ((-1) % 4)]
```
実根が2つ出てきた。
それぞれ、分離区間は \((-2,-1)\) と \((-1/2,-1/4)\) である。
これらに `a0`, `a1` と名前をつける：
```
> let [a0,a1] = it
```

まだ大した演算は実装していない。
いくつか比較演算を試してみよう。
```
> a0 < a1
True
> FromRat (-1.5) < a0
False
> FromRat (-1.83) < a0
True
> a0 < FromRat (-1.82)
True
```

`a0` を近似する区間の列を計算してみよう：
```
> take 10 $ intervals a0
[((-2) % 1,(-1) % 1),((-2) % 1,(-3) % 2),((-2) % 1,(-7) % 4),((-15) % 8,(-7) % 4),((-15) % 8,(-29) % 16),((-59) % 32,(-29) % 16),((-117) % 64,(-29) % 16),((-117) % 64,(-233) % 128),((-117) % 64,(-467) % 256),((-117) % 64,(-935) % 512)]
> (-117/64, -935/512)
(-1.828125,-1.826171875)
```
根の近似値として、それっぽい値が出てきたと言えるのではないだろうか。
