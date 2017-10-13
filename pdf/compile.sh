pandoc_opts="--from markdown+tex_math_single_backslash --to latex --template=template.tex"
stack exec pandoc -- ${pandoc_opts} -o 00-intro.tex ../posts/00-intro.md
stack exec pandoc -- ${pandoc_opts} -o 01-polynomial.tex ../posts/01-polynomial.md
stack exec pandoc -- ${pandoc_opts} -o 02-counting-real-roots.tex ../posts/02-counting-real-roots.md
stack exec pandoc -- ${pandoc_opts} -o 03-operations.tex ../posts/03-operations.md
