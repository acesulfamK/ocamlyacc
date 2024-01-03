# ocamlyacc
This repository is used as an introduction to ocaml yacc.


[Ocaml chapter1](https://v2.ocaml.org/manual/coreexamples.html#s%3Astandalone-programs)

[Official ocamllex and ocamlyacc](https://v2.ocaml.org/manual/lexyacc.html)

[Ocamlyacctutorial](https://ohama.github.io/ocaml/ocamlyacc-tutorial/concepts/)

# ocamllex

ocamllexでmllファイルをコンパイルすると、
mllのentrypointにつけた名前と同じ関数が定義されたmlファイルが得られる
(以降tokenという名前だったとする)。
tokenにlexbuf型の値を与えると、tokenが解析される。
lexbuf型の値の扱い方は[chap 26, Module Lexing](https://v2.ocaml.org/api/Lexing.html)にある。
```
> let my_lexbuf = Lexing.from_string "1+2+3"
> token my_lexbuf
```

[chap 26, Module Lexing](https://v2.ocaml.org/api/Lexing.html)

