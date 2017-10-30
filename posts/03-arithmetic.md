---
title: "#3 代数的数の演算と終結式"
date: 2017-10-28
---
\providecommand\Syl{\mathrm{Syl}}
\providecommand\resultant{\mathrm{res}}
\providecommand\gcd{\mathrm{gcd}}
\providecommand\leadingCoefficient{\mathrm{lc}}
\providecommand\transpose[1]{#1^T}

<a href="02-real-root-counting.html">前回</a>は代数的実数の比較演算を実装した。
今回は、代数的数の四則演算を考える。
今回の話題は代数的実数に限らず、代数的数にも当てはまる。

# 代数的数の四則演算

代数的数 \(\alpha\) と \(\beta\) に対し、それらの和 \(\alpha+\beta\), 差 \(\alpha-\beta\), 積 \(\alpha\beta\), 商 \(\alpha/\beta\) を根に持つ多項式を求めたい。

その前に用語と記号の確認をしておく。
代数的数 \(\alpha\) と \(\beta\) の定義多項式を、適切な拡大体において
\[
\begin{aligned}
f(x)&=a(x-\alpha_1)\cdots(x-\alpha_n), \\
g(x)&=b(x-\beta_1)\cdots(x-\beta_m)
\end{aligned}
\]
とおく。
\(f\), \(g\) が既約のとき（\(f\), \(g\) が最小多項式の時）、各 \(\alpha_i\) を \(\alpha\) の**共役** (conjugate)、\(\beta_j\) を \(\beta\) の共役と呼ぶ。

現時点ではまだ \(f\), \(g\) の既約性を判定する方法を知らないので \(\alpha_i\), \(\beta_j\) を共役とは呼べないが、そこは別にどうでもよくて、とにかく「同じ多項式の根となる仲間である」という点が重要である。

このとき、 \(\alpha+\beta\), \(\alpha-\beta\), \(\alpha\beta\), \(\alpha/\beta\) を根として持つ多項式を
\[
\begin{aligned}
h_{\alpha+\beta}(x)&=a^m b^n \prod_{i=1}^n \prod_{j=1}^m (x-(\alpha_i+\beta_j))=a^m b^n (x-(\alpha_1+\beta_1))\cdots(x-(\alpha_n+\beta_m)) \\
h_{\alpha-\beta}(x)&=a^m b^n \prod_{i=1}^n \prod_{j=1}^m (x-(\alpha_i-\beta_j))=a^m b^n (x-(\alpha_1-\beta_1))\cdots(x-(\alpha_n-\beta_m)) \\
h_{\alpha\beta}(x)&=a^m b^n \prod_{i=1}^n \prod_{j=1}^m (x-\alpha_i\beta_j)=a^m b^n (x-\alpha_1\beta_1)\cdots(x-\alpha_n\beta_m) \\
h_{\alpha/\beta}(x)&=a^m b^n \prod_{i=1}^n \prod_{j=1}^m (x-\alpha_i/\beta_j)=a^m b^n (x-\alpha_1/\beta_1)\cdots(x-\alpha_n/\beta_m) \quad (\beta\ne0)
\end{aligned}
\]
という風に構成できる。

一般に \(\alpha_i\) や \(\beta_j\) は元の係数環 \(R\) の元とは限らないが、それらの対称式（に適切に \(a\), \(b\) を乗じたもの）は \(R\) の元となる。
上に挙げた多項式 \(h_{\alpha\star\beta}\) の係数は \(\alpha_i\) および \(\beta_j\) の対称式で書けるので、 \(R\) 上の多項式である。

## 例題

\(\alpha=\sqrt{2}\), \(\beta=\sqrt{3}\) のとき、それぞれの最小多項式は
\(f(x)=x^2-2\), \(g(x)=x^2-3\) である。
まずは感覚を掴むため、\(\alpha+\beta\) の最小多項式を計算してみよう。

\[\begin{aligned}
&(x-(\sqrt{2}+\sqrt{3}))(x-(\sqrt{2}-\sqrt{3}))(x-(-\sqrt{2}+\sqrt{3}))(x-(-\sqrt{2}-\sqrt{3})) \\
=&(x-\sqrt{2}-\sqrt{3})(x-\sqrt{2}+\sqrt{3})(x+\sqrt{2}-\sqrt{3})(x+\sqrt{2}+\sqrt{3}) \\
=&((x-\sqrt{2})^2-3)((x+\sqrt{2})^2-3) \\
=&(x-\sqrt{2})^2(x+\sqrt{2})^2-3(x+\sqrt{2})^2-3(x-\sqrt{2})^2+9 \\
=&(x^2-2)^2-3(x^2+2\sqrt{2}x+2)-3(x^2-2\sqrt{2}x+2)+9 \\
=&(x^2-2)^2-6(x^2+2)+9 \\
=&x^4-4x^2+4-6x^2-12+9 \\
=&x^4-10x^2+1.
\end{aligned}\]

結果は整数係数となり、うまいこと根号が消えてくれた。

次は、 \(\alpha=\sqrt{2}\), \(\beta=\sqrt[3]{2}\) として \(\alpha+\beta\) の最小多項式を計算してみよう。
（今度は自分の手で計算して欲しい！）

...

手計算では手に負えないと感じて頂けただろうか？

\(h_{\alpha\star\beta}\) の係数を \(\alpha_i\) および \(\beta_j\) の対称式で書いて根と係数の関係を使う、という方針では、人間の手で計算するのも大変だし、機械で処理できるアルゴリズムを書き下すのも大変である。
もっと機械的に計算しやすい方法があると良い。
そのための強力な道具が、**終結式**である（別の道具として**グレブナー基底**があるが、今回は扱わない）。

# シルベスター行列と終結式

0 でない多項式 \(f(x)=a_nx^n+\cdots+a_0\in R[x]\), \(g(x)=b_mx^m+\cdots+b_0\in R[x]\)  に対し、 **シルベスター行列** (Sylvester matrix) \(\Syl(f,g)\) を次のように定義する：
\[
\Syl(f,g)=\begin{pmatrix}
a_n    &        & 0      & b_m    &        &        & 0 \\
\vdots & \ddots &        & \vdots & \ddots &        & \\
\vdots &        & a_n    & b_0    &        & \ddots & \\
a_0    &        & \vdots &        & \ddots &        & b_m \\
       & \ddots & \vdots &        &        & \ddots & \vdots \\
0      &        & a_0    & 0      &        &        & b_0
\end{pmatrix}
\]
\(\Syl(f,g)\) は \(n+m\) 次の正方行列で、左側の \(a_*\) の部分は \(m\) 列、右側の \(b_*\) の部分は \(n\) 列ある。
上に書いた行列の転置のことを \(\Syl(f,g)\) と呼ぶ流儀もある。

\(R\) が体のとき、次数が \(k\) 未満の多項式全体のなす \(k\) 次元線形空間を \(R[x]^{<k}\) と書くことにする。
その基底を \(1,x,x^2,\ldots,x^{k-1}\) と取れば、 \(\Syl(f,g)\) は線形写像
\[\begin{array}{cccc}
\varphi_{f,g}\colon&R[x]^{<m}\oplus R[x]^{<n} & \longrightarrow & R[x]^{<m+n} \\
&(s,t) & \longmapsto & sf+tg
\end{array}\]
の表現行列である。

この \(\varphi_{f,g}\) について、次が成り立つ：
<div class="theorem">
**補題**．
\(\varphi_{f,g}\) が同型写像であることと、 \(f\) と \(g\) が非自明な共通因子を持つこと（\(\gcd(f,g)\ne 1\) であること）は同値である。
</div>

\(\Syl(f,g)\) の行列式を \(\resultant(f,g)\) と書き、**終結式** (resultant) と呼ぶ：
\[\resultant(f,g)=\det\Syl(f,g)\in R\]
多項式についての GCD はやはり多項式であったのに対し、終結式は係数環 \(R\) の元となり、変数は消去される。

補題より、次が成り立つ：
<div class="theorem">
**定理**（終結式の基本性質）．
\(f\), \(g\) が非自明な共通因子を持つ (\(\gcd(f,g)\ne 1\)) とき、かつその時に限り \(\resultant(f,g)=0\).
</div>

なお、 \(f\) または \(g\) が定数のときも問題なく \(\resultant(f,g)\) は定義できる。
\(f\) が 0 でない定数 \(a\) のときは
\[
\Syl(f,g)=\begin{pmatrix}
a & & 0 \\
& \ddots & \\
0 & & a
\end{pmatrix}
\]
より \(\resultant(f,g)=a^m\) となり、 \(g\) が 0 でない定数 \(b\) のときは
\[
\Syl(f,g)=\begin{pmatrix}
b & & 0 \\
& \ddots & \\
0 & & b
\end{pmatrix}
\]
より \(\resultant(f,g)=b^n\) となる。
\(f\) と \(g\) が共に 0 でない定数の時は、「**0×0 行列の行列式は 1**」[^1]なので \(\resultant(f,g)=1\) となる。

[^1]: 0×0 行列の行列式については、[のらんぶる氏によるブログ記事](http://nolimbre.hateblo.jp/entry/2017/04/13/215730)を読むと良い。

\(f\) または \(g\) が 0 の時は厄介だが、「\(f\) と \(g\) が非自明な共通因子を持つときに限り \(\resultant(f,g)=0\)」という性質を拠り所とし、

- \(f=0\) で \(g\) が非 0 定数の場合、または \(g=0\) で \(f\) が非 0 定数の場合は \(\resultant(f,g)=1\),
- \(f=0\) で \(g\) が定数でない (\(\deg g\ge 1\)) 場合、または \(g=0\) で \(f\) が定数でない (\(\deg f\ge 1\)) 場合は \(\resultant(f,g)=0\),
- \(f=g=0\) の場合は \(\resultant(f,g)=0\)

とするのが一番筋が通っているように思われる（0 は任意の数で割り切れるため、任意の \(a\) に対して \(\gcd(a,0)=a\) となるということを思い出して欲しい）。
「\(f=0\) で \(g\) が 0,1 でない定数の場合」及び「\(g=0\) で \(f\) が 0,1 でない定数の場合」は \(\resultant(f,g)\) は不定とするべきな気もするが、まあ 0 に関する終結式が重要になることはないと思うので、気にしないことにする。

シルベスター行列も終結式も一変数多項式について定義されるものであるから、 \(f,g\) が多変数の多項式の場合は、どの変数についての（多項式係数）一変数多項式と見るかを明示する必要がある。
ここでは単に、消去される変数を下に書くことにする。
つまり、 \(f,g\in K[x,y]\) を \(f,g\in K[x][y]\) と見る場合の終結式は \(\resultant_y(f,g)\) と書き、これは \(K[x]\) の元である。

代数的数（つまり、多項式の根）と終結式を関連づける定理が次である。
線形代数の教科書を眺めると、しばしばこの定理が演習問題になっている。

<div class="theorem">
**定理**（終結式と根の関係）．
\(f(x)=a(x-\alpha_1)\cdots(x-\alpha_n)\), \(g(x)=b(x-\beta_1)\cdots(x-\beta_m)\) の時、
\[
\resultant(f,g)
=a^m \prod_{i=1}^n g(\alpha_i)
={({-1})}^{nm} b^n \prod_{j=1}^m f(\beta_j)
=a^m b^n \prod_{i=1}^n \prod_{j=1}^m (\alpha_i-\beta_j).
\]
</div>
<div class="proof">
証明．
根 \(\alpha_i\), \(\beta_j\) が不定元、つまり \(f,g\in R[A_1,\ldots,A_n,B_1,\ldots,B_m][x]\), \(f(x)=a(x-A_1)\cdots(x-A_n)\), \(g(x)=b(x-B_1)\cdots(x-B_m)\) として考える。
（\(f\) や \(g\) が重根を持っていると嫌なことになるので、一旦全て不定元と置くのである。）

各 \(i,j\) について、仮に \(A_i=B_j\) とすれば、終結式の基本性質より \(\resultant_x(f,g)=0\) となる。
すなわち、 \(\resultant_x(f,g)\) は \(A_i-B_j\) で割り切れる。

これが全ての \(i,j\) の組み合わせについて成り立つので、 \(\resultant_x(f,g)\) は \(\prod_{i=1}^n \prod_{j=1}^m (A_i-B_j)\) で割り切れる。
つまり、ある \(c\in R[A_1,\ldots,A_n,B_1,\ldots,B_m]\) について
\[\resultant_x(f,g)=c\prod_{i=1}^n \prod_{j=1}^m (A_i-B_j)\]
である。
あとは \(c\) を決定すれば良い。

さて、 \(A\) に関しての \(\resultant_x(f,g)\) の次数、つまり \(R'=R[B_1,\ldots,B_m]\) とおいて \(\resultant_x(f,g)\) を \(n\) 変数多項式環 \(R'[A_1,\ldots,A_n]\) の元として見た時の次数を考えると、これは \(nm\) である。
なぜなら、シルベスター行列の左半分の部分の中では \(a_0=({-1})^n a_n \prod_{i=1}^n A_i\) が一番 \(A\) に関しての次数が高く（\(n\) 次）、それらを全て掛け合わせる（左下の斜辺）と \(nm\) 次となるからである。
（シルベスター行列の右半分には \(A\) は含まれないので考慮しなくて良い）

このことを踏まえて \(\resultant_x(f,g)=c\prod_{i=1}^n \prod_{j=1}^m (A_i-B_j)\) を眺めると、 \(\prod_{i=1}^n \prod_{j=1}^m (A_i-B_j)\) の部分が \(A\) について \(nm\) 次なので、 \(c\) は \(A\) に依存しない定数であることが言える。

同様の議論で、 \(c\) は \(B\) に依存しない定数であることが言える。
つまり \(c\in R\) である。

今度は、全ての \(B_j\) に 0 を代入してみよう。
この時、右辺は \(c\prod_{i=1}^n A_i^m\) となる。
左辺は、行列式による定義より直接計算できて、
\[
\det\begin{pmatrix}
a_n    &        & 0      & b_m    &        &        & 0 \\
\vdots & \ddots &        & 0      & \ddots &        & \\
\vdots &        & a_n    & \vdots & \ddots & \ddots & \\
\vdots &        & \vdots & 0      &        & \ddots & b_m \\
a_0    &        & \vdots &        & \ddots &        & 0 \\
       & \ddots & \vdots &        &        & \ddots & \vdots \\
0      &        & a_0    & 0      &        &        & 0
\end{pmatrix}
=({-1})^{nm}a_0^m b_m^n
\]
となる。

根と係数の関係より \(a_0=({-1})^n a_n \prod_{i=1}^n A_i\) なので、
\[a_n^m b_m^n \prod_{i=1}^n A_i^m =c\prod_{i=1}^n A_i^m\]
であり、
\[c=a_n^m b_m^n\]
がわかる。

よって、
\[\resultant_x(f,g)=a_n^m b_m^n \prod_{i=1}^n \prod_{j=1}^m (A_i-B_j)\]
である。

具体的な \(f\) と \(g\) については、環準同型 \(A_i\mapsto\alpha_i\), \(B_j\mapsto\beta_j\) を適用すれば
\[
\resultant(f,g)
=a^m b^n \prod_{i=1}^n \prod_{j=1}^m (\alpha_i-\beta_j).
\]
を得る。
残りの2つの等式については明らかなので省略する。
</div>

定理より直ちに次が言える。
<div class="theorem">
**系**．
\[\resultant(f,gh)=\resultant(f,g)\resultant(f,h).\]
</div>

## 終結式の計算

本題に入る前に、終結式の計算についていくつか確認しておこう。

次の命題は、行列式の性質を使えば簡単にわかる。
<div class="theorem">
**命題**．
\(n=\deg f\ge 0\), \(m=\deg g\ge 0\) のとき、
\[\resultant(f,g)=({-1})^{nm}\resultant(g,f).\]
</div>

<div class="theorem">
**命題**．
\(n=\deg f\ge 0\), \(m=\deg g\ge 0\), \(a,b\in R\setminus\{0\}\) のとき、
\[\resultant(af,bg)=a^m b^n \resultant(f,g).\]
</div>

次の命題も、行列式の性質による。
ただし、 \(\leadingCoefficient\) で多項式の最高次の係数を表す。

<div class="theorem">
**命題**．
\(f,g,q\in R[x]\) に対し、 \(r:=f-qg\in R[x]\) が \(\deg r\le\deg f\) を満たすとする。
このとき、 \(r\ne 0\) ならば
\[\begin{aligned}
\resultant(f,g)&=({-1})^{(\deg f-\deg r)\deg g}\leadingCoefficient(g)^{\deg f-\deg r}\resultant(r,g) \\
{}&=({-1})^{\deg f \deg g}\leadingCoefficient(g)^{\deg f-\deg r}\resultant(g,r),
\end{aligned}\]
\(r=0\) ならば
\[\resultant(f,g)=\begin{cases}
0 & (\deg g\ge 1 \text{ または } g=0) \\
\leadingCoefficient(g)^{\deg f} & (\deg g=0)
\end{cases}\]
である。
</div>

特に、終結式の計算に、最大公約数を求める時に使うユークリッドの互除法と同様のアルゴリズムを利用できる。

# 終結式による代数的数の加減乗除

いよいよ本題である。

<div class="theorem">
**定理**．
\(n\) 次多項式 \(f\) と \(m\) 次多項式 \(g\) について、その根をそれぞれ重複を込めて \(\alpha_1,\ldots,\alpha_n\), \(\beta_1,\ldots,\beta_m\) とおく。
演算子 \(\star\) を \(+,-,\times,/\) のいずれかとして、
\[
\begin{aligned}
h_{\alpha\star\beta}(x)
&=\leadingCoefficient(f)^m\leadingCoefficient(g)^n(x-(\alpha_1\star\beta_1))(x-(\alpha_1\star\beta_2))\cdots(x-(\alpha_n\star\beta_m)) \\
&=\leadingCoefficient(f)^m\leadingCoefficient(g)^n\prod_{i=1}^n\prod_{j=1}^m (x-(\alpha_i\star\beta_j))
\end{aligned}
\]
と定めると、 \(h_{\alpha\star\beta}\) は終結式を使って次のように書ける：
\[
\begin{aligned}
h_{\alpha+\beta}(x)&=({-1})^{nm}\resultant_y(f(x-y),g(y))=({-1})^{nm}\resultant_y(g(x-y),f(y)), \\
h_{\alpha-\beta}(x)&=({-1})^{nm}\resultant_y(f(x+y),g(y))=\resultant_y(g(y-x),f(y)), \\
h_{\alpha\beta}(x)&=({-1})^{nm}\resultant_y(y^n f(x/y),g(y))=({-1})^{nm}\resultant_y(y^m g(x/y),f(y)), \\
h_{\alpha/\beta}(x)&=\left(\frac{\leadingCoefficient(g)}{g(0)}\right)^n\resultant_y(f(xy),g(y))
=({-1})^{nm}\left(\frac{\leadingCoefficient(g)}{g(0)}\right)^n\resultant_y(x^m g(y/x),f(y)).
\end{aligned}
\]
ただし、 \(\star=/\) の場合は \(g(0)\ne 0\) を仮定する。
</div>
<div class="proof">
証明．
和：
\[
\begin{aligned}
h_{\alpha+\beta}(x)
&=\leadingCoefficient(f)^m\leadingCoefficient(g)^n\prod_{i=1}^n \prod_{j=1}^m (x-(\alpha_i+\beta_j)) \\
&=\leadingCoefficient(f)^m\leadingCoefficient(g)^n\prod_{i=1}^n \prod_{j=1}^m ((x-\alpha_i)-\beta_j) \\
&=\leadingCoefficient(f)^m\prod_{i=1}^n g(x-\alpha_i) \\
&=\resultant_y(\leadingCoefficient(f)(y-(x-\alpha_1))\cdots(y-(x-\alpha_n))),g(y)) \\
%&=\resultant_y(\leadingCoefficient(f)(y-x+\alpha_1)\cdots(y-x+\alpha_n)),g(y)) \\
&=\resultant_y(({-1})^n\leadingCoefficient(f)(x-y-\alpha_1)\cdots(x-y-\alpha_n),g(y)) \\
{}&=({-1})^{nm}\resultant_y(f(x-y),g(y)).
\end{aligned}
\]
差 \(h_{\alpha-\beta}\) も同様に計算できる（読者への演習問題とする）。

積：
\[
\begin{aligned}
h_{\alpha\beta}(x)
&=\leadingCoefficient(f)^m\leadingCoefficient(g)^n\prod_{i=1}^n \prod_{j=1}^m (x-\alpha_i\beta_j) \\
&=\leadingCoefficient(f)^m\leadingCoefficient(g)^n\prod_{i=1}^n \prod_{j=1}^m \alpha_i (\alpha_i^{-1}x-\beta_j) \\
&=\leadingCoefficient(f)^m \prod_{i=1}^n \alpha_i^m g(\alpha_i^{-1}x) \\
&=\resultant_y(\leadingCoefficient(f)\left(\prod_{i=1}^n \alpha_i\right)(y-\alpha_1^{-1}x)\cdots(y-\alpha_n^{-1}x), g(y)) \\
&=\resultant_y(\leadingCoefficient(f)(\alpha_1y-x)\cdots(\alpha_ny-x), g(y)) \\
&=\resultant_y(({-y})^n\leadingCoefficient(f)(x/y-\alpha_1)\cdots(x/y-\alpha_n), g(y)) \\
{}&=({-1})^{nm}\resultant_y(y^nf(x/y), g(y)).
\end{aligned}
\]
商：
\[
\begin{aligned}
h_{\alpha/\beta}(x)
&=\leadingCoefficient(f)^m\leadingCoefficient(g)^n\prod_{i=1}^n \prod_{j=1}^m (x-\alpha_i/\beta_j) \\
&=\leadingCoefficient(f)^m\leadingCoefficient(g)^n\prod_{i=1}^n \prod_{j=1}^m \beta_j^{-1}(\beta_jx-\alpha_i) \\
&=\leadingCoefficient(g)^n\prod_{j=1}^m \beta_j^{-n}f(\beta_jx) \\
%&=\left(\prod_{j=1}^m \beta_j\right)^{-n}\resultant_y(\leadingCoefficient(g)(y-\beta_1x)\cdots(y-\beta_mx),f(y)) \\
%&=\left(\prod_{j=1}^m \beta_j\right)^{-n}\resultant_y(x^m\leadingCoefficient(g)(y/x-\beta_1)\cdots(y/x-\beta_m),f(y)) \\
%{}&=({-1})^{nm}\left(\frac{\leadingCoefficient(g)}{g(0)}\right)^n\resultant_y(x^m g(y/x),f(y)). \\
&=({-1})^{nm}\left(\prod_{j=1}^m \beta_j\right)^{-n}\resultant_y(f(xy),\leadingCoefficient(g)(y-\beta_1)\cdots(y-\beta_m)) \\
{}&=\left(\frac{\leadingCoefficient(g)}{g(0)}\right)^{n}\resultant_y(f(xy),g(y)).
\end{aligned}
\]
</div>

なお、一方が有理数の場合の演算は次のようになる：
<div class="theorem">
**定理**．
\(k\) を有理数とする。
\(n\) 次多項式 \(f\) について、その根をそれぞれ重複を込めて \(\alpha_1,\ldots,\alpha_n\) とおく。
\(F(x)\) を代数的数に関する関数 \(x+k, k-x, kx, 1/x\) のいずれかとして、
\[
\begin{aligned}
h_{F(\alpha)}(x)
&=\leadingCoefficient(f)(x-F(\alpha_1))(x-F(\alpha_1))\cdots(x-F(\alpha_n)) \\
&=\leadingCoefficient(f)\prod_{i=1}^n (x-F(\alpha_i))
\end{aligned}
\]
と定めると、 \(h_{F(\alpha)}\) は次のように書ける：
\[
\begin{aligned}
h_{\alpha+k}(x)&=f(x-k), \\
h_{k-\alpha}(x)&=f(k-x), \\
h_{k\alpha}(x)&=f(x/k), \\
h_{1/\alpha}(x)&=\frac{\leadingCoefficient(f)}{f(0)}x^nf(1/x).
\end{aligned}
\]
ただし、 \(F(x)=1/x\) の場合は \(f(0)\ne 0\) を仮定する。
</div>
このように、代数的数が1個でもう一方が有理数の場合は、終結式を使わずに計算できる。

# 終結式の実装

互除法を使って終結式を実装してみよう：

```haskell
{-# LANGUAGE BangPatterns #-}
module Numeric.AlgebraicReal.Resultant where
import Numeric.AlgebraicReal.UniPoly

-- | 体係数多項式の終結式
resultant :: (Eq a, Fractional a) => UniPoly a -> UniPoly a -> a
resultant f g
  | (f == 0 && degree g == Just 0) || (degree f == Just 0 && g == 0) = 1
  | f == 0 || g == 0 = 0
  | degree' f == 0 = leadingCoefficient f ^ degree' g
  | otherwise = loop 1 f g
  where
    -- invariant: loop p f g = p * resultant f g, f /= 0, g /= 0
    loop p f g
      | degree' g == 0 = p * lc_g ^ degree' f
      | r == 0 = 0
      | otherwise = let !s = degree' g * degree' f
                        !j = degree' f - degree' r
                    in loop ((-1)^s * lc_g^j * p) g r
      where r = f `modP` g
            lc_g = leadingCoefficient g
```

上記の内容を `src/Numeric/AlgebraicReal/Resultant.hs` として保存し、 `algebraic-real.cabal` の
```
  exposed-modules:     Numeric.AlgebraicReal.UniPoly
                     , Numeric.AlgebraicReal.AlgReal
```
を
```
  exposed-modules:     Numeric.AlgebraicReal.UniPoly
                     , Numeric.AlgebraicReal.AlgReal
                     , Numeric.AlgebraicReal.Resultant
```
に書き換える。
それができたら `stack repl` を実行して、終結式の計算を試してみよう。
```
$ stack repl
...略...
Ok, modules loaded: Numeric.AlgebraicReal.AlgReal, Numeric.AlgebraicReal.Resultant, Numeric.AlgebraicReal.UniPoly.
Loaded GHCi configuration from .../ghci-script
*Numeric.AlgebraicReal.UniPoly Numeric.AlgebraicReal.AlgReal Numeric.AlgebraicReal.Resultant Numeric.AlgebraicReal.UniPoly> :set prompt "> "
> let x = ind
> resultant (x^2 + 2*x + 1) (x^3 + 3*x) :: Rational
16 % 1
> resultant (x + 1) (x^3 + 2*x + 1) :: Rational
(-2) % 1
```
それっぽい値が出てきた。

演習問題：手計算でシルベスター行列の行列式を計算することにより、 \(\resultant(x^2+2x+1,x^3+3x)=16\) と \(\resultant(x+1,x^3+2x+1)={-2}\) が正しいことを確かめよ。

# 代数的数の四則演算の実装にあたっての課題

さて、体係数多項式同士の終結式を求めるならこれでいいのだが、今回計算したいのは多項式を係数とする多項式同士の終結式である。
しかし、多項式環は体ではないので、それを係数とする多項式除算はできない。
すると互除法のアルゴリズムがそのままでは使えない。
困った。

さすがに、終結式が行列式によって定義されているからと言って、行列式を置換を使った式で展開するようなことはしたくない。
互除法をベースにした方法を使いたい。
そのためには

- 有理関数体で計算する
- **擬除算**を使う

という方針が考えられる。
これらは次回以降にする。

さて、終結式によって（演算結果を根に持つ）多項式を計算できたとしても、まだ分離区間を求めるという作業が必要である。
というのは、 \(\alpha\) の分離区間が \((a,b)\), \(\beta\) の分離区間が \((c,d)\) だからと言って、 \((a+c,b+d)\) が \(\alpha+\beta\) の分離区間となるとは限らないからだ。
もちろん \(\alpha+\beta\) は区間 \((a+c,b+d)\) に含まれるが、その区間には他の \(\alpha_i+\beta_j\) が含まれるかもしれない。

例えば、\(\alpha=\sqrt{3}\), \(\beta=\frac{\sqrt{2}}{10}\) のとき、前者の分離区間は \((1,2)\), 後者の分離区間は \((0,1)\) と取れる。
しかし、その区間を単純に足したもの \((1+0,2+1)=(1,3)\) には、求めたい値 \(\sqrt{3}+\frac{\sqrt{2}}{10}\approx 1.87\) の他にその共役である \(\sqrt{3}-\frac{\sqrt{2}}{10}\approx 1.59\) も含まれてしまう。

つまり、正しい分離区間を得るには、 \(\alpha\) と \(\beta\) の区間を「十分狭く」取ってやり、それを足すという操作が必要になる。
「十分狭く」というところが厄介だが、

- \(h_{\alpha+\beta}\) の実根 \(\gamma_k\) の距離の最小値 \(\min_{k,l} \left\lvert{\gamma_k-\gamma_l}\right\rvert\) を求めて、足した区間の幅がそれ以下になるようにする。
- \(\alpha\), \(\beta\) について、それぞれに収束する縮小区間列 \(\{(a_0,b_0),(a_1,b_1),\ldots\}\), \(\{(c_0,d_0),(c_1,d_1),\ldots\}\) を考える。その和 \(\{(a_0+c_0,b_0+d_0),(a_1+c_1,b_1+d_1),\ldots\}\) は \(\alpha+\beta\) に収束する縮小区間列なので、いずれ分離区間となる（分離区間かどうか判定するには、前回やったスツルムの定理を使う）。

という方針が考えられる。
これも次回以降とする。
