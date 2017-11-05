---
title: "#4 擬除算と多項式剰余列"
date: 2017-11-05
---
\providecommand\Syl{\mathrm{Syl}}
\providecommand\resultant{\mathrm{res}}
\providecommand\gcd{\mathrm{gcd}}
\providecommand\degree{\mathrm{deg}}
\providecommand\leadingCoefficient{\mathrm{lc}}
\providecommand\abs[1]{\left\lvert #1\right\rvert}
\providecommand\prem{\mathrm{prem}}
\providecommand\content{\mathrm{cont}}
\providecommand\primitivePart{\mathrm{pp}}

[前回](03-arithmetic.html)は、多項式係数の多項式の終結式を求めたい、という話で終わった。
今回は、終結式の計算について、もう少し深く掘り下げる。

ただし、前回挙げた「有理関数体で考える」という選択肢は考えない（有理関数の計算においては毎回、約分のコストがかかるため）。
今回は、基本的に、分数を使わないような計算方法を取り上げる。

# 整域係数多項式の終結式の計算

## 擬除算

前回の終結式計算で困難となったのは、「多項式係数の多項式には、普通の多項式除算（ユークリッド除算）が使えない」という点だった。
多項式係数に限らず、整数係数の多項式も同じ問題を抱えている。
そこで、しばらくの間は整数係数多項式について、多項式除算の議論しよう。

例えば、整数係数多項式 \(f(x)=x^2+3x+1\) を \(g(x)=2x+1\) で割ってみると、
\[x^2+3x+1=\frac{2x+5}{4}\cdot(2x+1)-\frac{1}{4}\]
となり、商 \(\frac{2x+5}{4}\) と余り \(-\frac{1}{4}\) に分数が出現する。

しかし、両辺に 4 をかければ、
\[4\cdot(x^2+3x+1)=(2x+5)\cdot(2x+1)-1\]
という風に、「商っぽいもの」と「余りっぽいもの」が整数係数多項式として出てくる。

演習問題：\(f(x)=x^3+3x^2-x+4\) を \(g(x)=3x-2\) で割った商と余りを計算し、その両辺に適当な数をかけることにより整数係数にせよ。
<!--
別の例で考えてみよう。
\(f(x)=x^3+3x^2-x+4\) を \(g(x)=3x-2\) で割ってみると、
\[x^3+3x^2-x+4=\frac{9x^2+33x+13}{27}\cdot(3x-2)+\frac{132}{27}\]
となり、両辺に 27 をかけると
\[27\cdot(x^3+3x^2-x+4)=(9x^2+33x+13)\cdot(3x-2)+132\]
を得る。
-->

勘の鋭い人は気づいたかもしれないが、両辺にかける数というのは \(g\) の最高次の係数のべき乗となっている。
つまり、整域 \(R\) において、 \(f\in R[x]\) と \(g\in R[x]\) の「割り算っぽいもの」は、適当な自然数 \(k\) について
\[\leadingCoefficient(g)^k f=qg+r, \quad q,r\in R[x], \deg r < \deg f\]
の形で書ける。

多項式除算のアルゴリズムを考えれば、この \(k\) は、必ず \(\deg f-\deg g+1\) 以下に取れる。
特に \(k=\deg f-\deg g+1\) とする場合、この「割り算っぽいもの」を**擬除算** (pseudodivision) という。
擬除算はいつでもできる。

以下、 \(f\) を \(g\) で割った時の余りを \(\prem(f,g)\) で書く。
つまり、 \(\prem(f,g)\) は
\[\leadingCoefficient(g)^{\deg f-\deg g+1} f=qg+, \quad q,r\in R[x], \deg r < \deg f\]
となる \(r\) のことである。

Haskell では擬除算を次のように実装できる：
```haskell
-- UniPoly.hs に追加
pseudoDivModP :: (Eq a, Num a) => UniPoly a -> UniPoly a -> (UniPoly a, UniPoly a)
pseudoDivModP f g
  | g == 0 = error "pseudoDivModP: divide by zero"
  | degree f < degree g = (zeroP, f)
  | otherwise = case loop 0 zeroP f of
      (i,q,r) -> (scaleP (b^(l-i)) q, scaleP (b^(l-i)) r)
  where
    l = degree' f - degree' g + 1
    b = leadingCoefficient g
    -- invariant: scaleP i f == q * g + r
    loop i q r | degree r < degree g = (i, q, r)
               | otherwise = let q' = UniPoly (V.drop (degree' g) (coeff r))
                             in loop (i + 1) (scaleP b q + q') (scaleP b r - q' * g)

pseudoDivP f g = fst (pseudoDivModP f g)
pseudoModP f g = snd (pseudoDivModP f g)
```

## 終結式の計算

体係数の多項式 \(f,g\) の終結式の計算では、剰余 \(r:=f-qg\) を使って
\[\begin{aligned}
\resultant(f,g)&=\resultant(qg+r,g) \\
&=({-1})^{(\deg f-\deg r)\deg g}\leadingCoefficient(g)^{\deg f-\deg r}\resultant(r,g) \\
{}&=({-1})^{\deg f \deg g}\leadingCoefficient(g)^{\deg f-\deg r}\resultant(g,r)
\end{aligned}\]
という風に多項式の次数を落とせるのだった。

整域係数の場合は、
\[\leadingCoefficient(g)^k f=qg+r, \quad \deg r < \deg f\]
となる \(k\) と \(r\) が取れるので、これを使うと
\[\begin{aligned}
\resultant(f,g)&=\resultant(\leadingCoefficient(g)^{-k}(qg+r),g) \\
&=({-1})^{(\deg f-\deg r)\deg g}\leadingCoefficient(g)^{\deg f-\deg r}\resultant(\leadingCoefficient(g)^{-k}r,g) \\
&=({-1})^{(\deg f-\deg r)\deg g}\leadingCoefficient(g)^{\deg f-\deg r-k\deg g}\resultant(r,g) \\
{}&=({-1})^{\deg f \deg g}\leadingCoefficient(g)^{\deg f-\deg r-k\deg g}\resultant(g,r),
\end{aligned}\]
とできる。
ここで問題となるのが、 \(\leadingCoefficient(g)^{\deg f-\deg r-k\deg g}\) の指数 \(\deg f-\deg r-k\deg g\) が負になる可能性があるという点である。
今回はなるべく整域（整数や多項式環）だけで計算したいという話なので、マイナス乗はできない。

幸い、終結式に関しては、最終的には値が元の係数環（整数や多項式環）に収まるということがわかっているので、
\(\deg f-\deg r-k\deg g<0\) の場合でも \(\resultant(g,r)\) は \(\leadingCoefficient(g)^{\abs{\deg f-\deg r-k\deg g}}\) で（余りなしで）割り切れる。
この商を \(\resultant(f,g)\) の計算結果とすれば良い。

プログラミング言語でこのように実装する場合は再帰を使うことになる。

```haskell
-- Resultant.hs に追記
resultant_int :: UniPoly Integer -> UniPoly Integer -> Integer
resultant_int f g
  | (f == 0 && degree g == Just 0) || (degree f == Just 0 && g == 0) = 1
  | f == 0 || g == 0 = 0
  | degree' f == 0 = leadingCoefficient f ^ degree' g
  | degree' g == 0 = leadingCoefficient g ^ degree' f
  | r == 0 = 0
  | degree' f >= degree' g, l >= 0 = (-1)^(degree' f * degree' g) * lc_g^l * resultant_int g r
  | degree' f >= degree' g, l < 0  = (-1)^(degree' f * degree' g) * resultant_int g r `div` lc_g^(-l)
  | otherwise = (-1)^(degree' f * degree' g) * resultant_int g f
  where
    r = pseudoModP f g
    lc_g = leadingCoefficient g
    l = degree' f - degree' r - (degree' f - degree' g + 1) * degree' g
```
この実装に div 関数を使っているが、 Haskell 標準には整域に対応する型クラスが用意されていないので、一般の整域を係数とする多項式の終結式を計算する関数を書くことはできない。
つまり、多項式係数の終結式を使いたかったら似たような関数を別に書く必要がある。
もちろん、整域に対応する型クラスを作れば、一つの実装を整数係数と多項式係数で使いまわすことができる。

実行してみよう：
```haskell
$ stack repl
...略...
*Numeric.AlgebraicReal.UniPoly Numeric.AlgebraicReal.AlgReal Numeric.AlgebraicReal.Resultant Numeric.AlgebraicReal.UniPoly> :set prompt "> "
> let x = ind
> resultant_int (x^2 + 2*x + 1) (x^3 + 3*x)
16
> resultant_int (x + 1) (x^3 + 2*x + 1)
-2
```
前回と同じ結果が、整数として得られた（前回は有理数体の上で計算した）。

もう少し複雑な多項式の終結式も計算してみよう。
```haskell
> resultant_int (x^4 + 3*x^3 - 2*x + 4) (x^3 - 7*x^2 + x - 1)
49218
> resultant_int (3*x^7 + x^5 + 2*x^4 - 2) (2*x^5 - 3*x^3 + 7)
629446012
> resultant_int (2*x^8 + x^5 - 3) (3*x^5 + x^2)
-1594332
> resultant_int (x^4 + 2*x^2 + 7*x + 1) (x^3 + x + 7)
49
```

再帰ではなくループで実装する、あるいは同じことだが末尾再帰として書くには、途中の係数の「分子」と「分母」をアキュムレーターとして、それに \(\leadingCoefficient(g)^{\abs{\deg f-\deg r-k\deg g}}\) をかけていくようにすれば良い。

演習問題：体係数多項式を係数とする多項式の終結式を計算する関数
`resultant_poly :: (Eq a, Fractional a) => UniPoly (UniPoly a) -> UniPoly (UniPoly a) -> UniPoly a`{.haskell}
を実装せよ。

演習問題：（続き）実装した `resultant_poly` 関数を使い、 `resultant_poly ((constP ind - ind)^2 - 2) (ind^2 - 3)`{.haskell} を計算せよ。
この終結式は何を意味しているか、前回の記事を見ながら考えよ。

## 素朴な方法の課題

再帰で計算するか、ループ（末尾再帰）でアキュムレーターを使って計算するかだが、プログラマーの常識的には後者を選びたくなるのが人情だろう。
しかし、分子と分母をアキュムレーターとする方式では、分子分母が巨大となってしまう恐れがある。
その都度最大公約数を計算して約分するという手もあるが、そもそも分数の計算を避けたかったはずなので、これでは余り嬉しくない。

# ユークリッドの互除法再論

終結式は一旦置いておいて、互除法の話をしよう。

\#1 でユークリッドの互除法による多項式の GCD の計算を紹介した時、「係数膨張」という現象が起こることを見た。
その時は緩和策として、「モニック多項式を使う」というものを紹介した。

しかし、モニック多項式といえど有理数計算を伴う。
整数係数だけで係数膨張を緩和する方法はないか？

## 多項式剰余列

ユークリッドの互除法には、
\[P_1=f, P_2=g, P_{i+1}=\text{〈}P_{i-1} \text{を} P_{i} \text{で割った余り〉}\]
という関係の多項式の列が登場した。

整数係数や多項式係数の場合は、「割った余り」に何か適当なものをかけて分数をなくすだそう。
あるいは、割った余りの係数が巨大になった場合は、係数をその公約数で割るという操作をするかもしれない。
多項式の剰余を取る列について、定数倍（または定数で割る操作）を許した一般化を、**多項式剰余列** (PRS; polynomial remainder sequence) という。

つまり、 \(f,g\in R[x]\) の多項式剰余列とは、ある 0 でない \(a_{i+1}, b_{i+1}\in R\) について
\[P_1=f, \quad P_2=g, \quad a_{i+1}P_{i+1}=b_{i+1}P_{i-1}-Q_{i+1}P_{i}\]
となる \(P_1,P_2,\ldots,P_k\) のことである。
次数については \(\deg f>\deg g\) を仮定し、単調減少となるようにする。

例えば、擬除算を使って \(P_{i+1}=\prem(P_{i-1},P_i)\) と構成した列は多項式剰余列となる。
この剰余列は当然係数膨張を起こす。

実装例：
```haskell
-- 整数係数多項式の、擬除算による剰余列を計算する
pseudoEuclidPRS :: (Eq a, Num a) => UniPoly a -> UniPoly a -> [UniPoly a]
pseudoEuclidPRS _ 0 = []
pseudoEuclidPRS f g = case pseudoModP f g of
  0 -> []
  rem -> rem : pseudoEuclidPRS g rem
```

実行例（`resultant_int` の実行例と同様、 `let x = ind` を実行済みだとする）：
```haskell
> pseudoEuclidPRS (x^4 + 3*x^3 - 2*x + 4) (x^3 - 7*x^2 + x - 1)
[UniPoly [14,-11,69],UniPoly [1847,-1397],UniPoly [234326898]]
> pseudoEuclidPRS (3*x^7 + x^5 + 2*x^4 - 2) (2*x^5 - 3*x^3 + 7)
[UniPoly [-170,0,-84,66,16],UniPoly [-20648,5440,-11088,10632],UniPoly [-1064632320,-1269940224,-673038336],UniPoly [12966504262418313510912,21469835377008458072064],UniPoly [-250367415553521117559011866972745726771418112062390272]]
> pseudoEuclidPRS (2*x^8 + x^5 - 3) (3*x^5 + x^2)
[UniPoly [-243,0,-9],UniPoly [-177147,14348907],UniPoly [-50031827528536188]]
> pseudoEuclidPRS (x^4 + 2*x^2 + 7*x + 1) (x^3 + x + 7)
[UniPoly [1,0,1],UniPoly [7]]
```

## 原始剰余列

係数膨張は係数に余計な共通因数が含まれるために起こる。
ならば、剰余列に現れる多項式をその係数の最大公約数で割れば良い。
そうして得られる多項式剰余列が**原始剰余列** (primitive PRS) である。

いくつか用語を定義しよう。
多項式 \(f\) の**内容** (content) \(\content(f)\) とは、係数の最大公約数のことである。
多項式 \(f\) の**原始部分** (primitive part) \(\primitivePart(f)\) とは、多項式をその内容で割ったものである。
\[\begin{aligned}
\content(f)&=\gcd\{a_0,\ldots,a_n\}, \\
\primitivePart(f)&=f/\content(f).
\end{aligned}\]

例えば、多項式 \(-2x+4\) の内容は 2 で、原始部分は \(-x+2\) である（最大公約数は正とする）。

内容と原始部分は Haskell では次のように実装できる：

```haskell
-- UniPoly.hs に追加

-- 整数係数多項式の内容を計算する
content_int :: UniPoly Integer -> Integer
content_int (UniPoly xs) = gcdV 0 xs -- 短絡評価を考えなければ foldr gcd 0 xs でも良い
  where
    -- foldl/foldr と gcd の組み合わせでは GCD が 1 になっても残りの部分が評価される。
    -- 列の途中で GCD が 1 になれば全体の GCD は 1 で確定なので、そういう短絡評価する。
    gcdV :: Integer -> V.Vector Integer -> Integer
    gcdV 1 _ = 1
    gcdV a v | V.null v = a
             | otherwise = gcdV (gcd (V.last v) a) (V.init v)

-- 整数係数多項式の内容と原始部分を計算する
contentAndPrimitivePart_int :: UniPoly Integer -> (Integer, UniPoly Integer)
contentAndPrimitivePart_int f@(UniPoly xs)
  | c == 1 = (c, f)
  | otherwise = (c, UniPoly (V.map (`div` c) xs))
  where c = content_int f

-- 整数係数多項式の原始部分を計算する
primitivePart_int :: UniPoly Integer -> UniPoly Integer
primitivePart_int = snd . contentAndPrimitivePart_int
```
（Haskell 標準の div 関数や gcd 関数は整数に関してしか使えないので、係数の型を Integer 専用にしている。ユークリッド整域に対応する型クラスを作れば、多項式係数の場合の内容、原始部分の実装も共通化できる。）

これを使うと、多項式の原始剰余列は次のように実装できる：

```haskell
-- 整数係数多項式の原始剰余列を計算する
primitivePRS_int :: UniPoly Integer -> UniPoly Integer -> [UniPoly Integer]
primitivePRS_int _ 0 = []
primitivePRS_int f g = case pseudoModP f g of
  0 -> []
  rem -> let !r' = primitivePart_int rem in r' : primitivePRS_int g r'
```

実行例：
```haskell
> primitivePRS_int (x^4 + 3*x^3 - 2*x + 4) (x^3 - 7*x^2 + x - 1)
[UniPoly [14,-11,69],UniPoly [1847,-1397],UniPoly [1]]
> primitivePRS_int (3*x^7 + x^5 + 2*x^4 - 2) (2*x^5 - 3*x^3 + 7)
[UniPoly [-85,0,-42,33,8],UniPoly [-2581,680,-1386,1329],UniPoly [-21660,-25837,-13693],UniPoly [3418559,5660423],UniPoly [-1]]
> primitivePRS_int (2*x^8 + x^5 - 3) (3*x^5 + x^2)
[UniPoly [-27,0,-1],UniPoly [-1,81],UniPoly [-1]]
> primitivePRS_int  (x^4 + 2*x^2 + 7*x + 1) (x^3 + x + 7)
[UniPoly [1,0,1],UniPoly [1]]
```

原始剰余列は、整数係数の範囲では究極に係数膨張を抑えた代物である。
しかし、係数の最大公約数を求めるという操作を毎回行うのは、いかにも非効率そうである[^1]。
そこで Collins が導入したのが、簡約剰余列と部分終結式剰余列である。

[^1]: その後の研究によると、実は、原始部分の計算はそれほど大変ではなく、原始剰余列の方が部分終結式剰余列よりも優れているらしい。（しかしそれも五十歩百歩で、結局はモジュラー計算の方が優れているようだ。）

## 簡約剰余列

**簡約剰余列** (reduced PRS) \(P_1,P_2,\ldots,P_k\) は次のように計算できる列である：
\[\beta_i P_{i}=\prem(P_{i-2},P_{i-1}),\quad i=3,\ldots,k\]
ただし \(\beta_i\) は
\[\begin{aligned}
\beta_3&=1, \\
\beta_i&=c_{i-2}^{\delta_{i-3}+1}, \quad i=4,\ldots,k
\end{aligned}\]
で定まり、 \(c_i\) と \(\delta_i\) は
\[c_i=\leadingCoefficient(P_i)\]
\[\delta_i=\degree P_i-\degree P_{i+1}, \quad i=1,\ldots,k-1\]
である。

ここでは証明しないが、 \(\prem(P_{i-2},P_{i-1})\) が \(\beta_i\) で割り切れるというのがポイントである。

<!--多項式剰余列の次数が1ずつ減っていく場合（normal な場合）は、符号を除いて部分終結式剰余列と一致する。-->

実装例：
```haskell
reducedPRS_int :: UniPoly Integer -> UniPoly Integer -> [UniPoly Integer]
reducedPRS_int _ 0 = []
reducedPRS_int f g = case pseudoModP f g of
  0 -> []
  rem -> rem : loop (degree' f) g rem
  where
    loop :: Int -> UniPoly Integer -> UniPoly Integer -> [UniPoly Integer]
    loop !deg_h f g = case pseudoModP f g of
      0 -> []
      rem -> let !deg_f = degree' f
                 !beta = leadingCoefficient f ^ (deg_h - deg_f + 1)
                 !mr = mapCoeff (`div` beta) rem
             in mr : loop deg_f g mr
```

ただし、 `mapCoeff` 関数は次のように定義される：
```haskell
-- UniPoly.hs
-- 多項式の係数に関数を適用する
mapCoeff :: (Eq b, Num b) => (a -> b) -> UniPoly a -> UniPoly b
mapCoeff f = fromCoeff . fmap f . coeff
```

実行例：
```haskell
> reducedPRS_int (x^4 + 3*x^3 - 2*x + 4) (x^3 - 7*x^2 + x - 1)
[UniPoly [14,-11,69],UniPoly [1847,-1397],UniPoly [49218]]
> reducedPRS_int (3*x^7 + x^5 + 2*x^4 - 2) (2*x^5 - 3*x^3 + 7)
[UniPoly [-170,0,-84,66,16],UniPoly [-2581,680,-1386,1329],UniPoly [-64980,-77511,-41079],UniPoly [3418559,5660423],UniPoly [-629446012]]
> reducedPRS_int (2*x^8 + x^5 - 3) (3*x^5 + x^2)
[UniPoly [-243,0,-9],UniPoly [-2187,177147],UniPoly [-1162268028]]
> reducedPRS_int  (x^4 + 2*x^2 + 7*x + 1) (x^3 + x + 7)
[UniPoly [1,0,1],UniPoly [7]]
```

擬除算による剰余列と比べると、（原始剰余列には及ばないにしろ）かなり係数膨張を抑えられたと言えるのではないだろうか。

しかし、簡約剰余列は部分終結式剰余列に比べて特にメリットがないのか、その後の研究ではあまり掘り下げられていない。

## 部分終結式剰余列

**部分終結式剰余列** (subresultant PRS) \(P_1,P_2,\ldots,P_k\) は次のように計算できる列である：
\[\beta_i P_{i}=\prem(P_{i-2},P_{i-1}),\quad i=3,\ldots,k\]
ただし \(\beta_i\) は
\[\begin{aligned}
\beta_3&=({-1})^{\delta_1+1}, \\
\beta_i&=-c_{i-2}\psi_i^{\delta_{i-2}}, \quad i=4,\ldots,k
\end{aligned}\]
で定まり、 \(c_i\) と \(\delta_i\) と \(\psi_i\) は
\[c_i=\leadingCoefficient(P_i)\]
\[\delta_i=\degree P_i-\degree P_{i+1}, \quad i=1,\ldots,k-1\]
\[\begin{aligned}
\psi_3&={-1}, \\
\psi_i&=({-c_{i-2}})^{\delta_{i-3}}\psi_{i-1}^{1-\delta_{i-3}}, \quad i=4,\ldots,k
\end{aligned}\]
により定まる。

ここでは証明しないが、 \(\beta_i\) と \(\psi_i\) は係数環 \(R\) の元となり、また、 \(P_{i}\) は \(R\) 係数となる。
（Collins の原論文では \(\psi\) を使わずに \(\beta\) に相当するものを直接 \(c_i\) の積として書いている。\(\psi\) を使うこの形は Brown, Traub に因る。）

これらの \(P_i\) は**部分終結式** (subresultant) と深い関わりがある。
（TODO: 部分終結式の説明）

実装例：
```haskell
subresultantPRS_int :: UniPoly Integer -> UniPoly Integer -> [UniPoly Integer]
subresultantPRS_int _ 0 = []
subresultantPRS_int f g = case pseudoModP f g of
  0 -> []
  rem -> let !d = degree' f - degree' g
             !s = (-1)^(d + 1) * rem
         in s : loop d (-1) g s
  where
    loop _ _ _ 0 = []
    loop d psi f g = case pseudoModP f g of
      0 -> []
      rem -> let !d' = degree' f - degree' g
                 !c = leadingCoefficient f
                 !psi' = (-c)^d `div` psi^(d-1)
                 !beta = -c * psi' ^ d'
                 !s = mapCoeff (`div` beta) rem
             in s : loop d' psi' g s
```

実行例：
```haskell
> subresultantPRS_int (x^4 + 3*x^3 - 2*x + 4) (x^3 - 7*x^2 + x - 1)
[UniPoly [14,-11,69],UniPoly [1847,-1397],UniPoly [49218]]
> subresultantPRS_int (3*x^7 + x^5 + 2*x^4 - 2) (2*x^5 - 3*x^3 + 7)
[UniPoly [170,0,84,-66,-16],UniPoly [-2581,680,-1386,1329],UniPoly [64980,77511,41079],UniPoly [3418559,5660423],UniPoly [629446012]]
> subresultantPRS_int (2*x^8 + x^5 - 3) (3*x^5 + x^2)
[UniPoly [-243,0,-9],UniPoly [-3,243],UniPoly [-1594332]]
> subresultantPRS_int (x^4+2*x^2+7*x+1) (x^3+x+7)
[UniPoly [1,0,1],UniPoly [7]]
```

実行例を見ると、最初の2例および4番目の例については、簡約剰余列と符号を除いて一致していることがわかる。
そして、最初の3例については、剰余列の最後の項が終結式と一致している（`resultant_int` の実行例と比較してみよう）。

実際、部分終結式剰余列の最後の項が定数でその一個前の項が1次であれば、その最後の項は終結式と一致する（例によって証明は省略する）。

本当は終結式と部分終結式剰余列との関係をもっと掘り下げられれば良かったのだが、筆者の現時点での力量が不足しているため、今回の話はここまでとする。

# 参考文献

- G. E. Collins, *Subresultants and Reduced Polynomial Remainder Sequences*, Journal of the ACM, Vol. 14, No. 1, Jan 1967, pp. 128-142.
    - reduced PRS と subresultant PRS を導入し、それらを計算するアルゴリズムを与えている。
- W. S. Brown and J. F. Traub, *On Euclid's Algorithm and the Theory of Subresultants*, Journal of the ACM, Vol. 18, No. 4, Oct 1971, pp. 505-514
    - 上記 Collins の論文の subresultant PRS に関する証明をわかりやすくしている。

\#0 で挙げた書籍に関して言うと、 Modern Computer Algebra 3rd ed. には、簡約剰余列と部分終結式剰余列は（歴史的経緯の項目を除いて）載っていない。
モジュラー計算の方が実用的なので部分終結式剰余列は必要ないという判断だろうか。

一方、 TAoCP Vol.2 には、部分終結式剰余列のアルゴリズムが載っている。
