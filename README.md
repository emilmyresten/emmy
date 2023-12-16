### Emmy
Tree-walking emmyreter implementation of a LISP-type language implemented in OCaml. Toy project. Syntax inspired by Clojure. 

`Calculate the nth fibonacci number`:
![](assets/fibonacci.png)

`Calculate the number of iterations before the Collatz equation reaches 1:`
![](assets/collatz.png)

### Build & run:

`opam install --deps-only --with-test .`

`dune exec emmy`

Run programs using

`dune exec emmy ./programs/main.emmy`

#### Tests:
Testing is done with [alcotest](https://github.com/mirage/alcotest).

`dune runtest`

### Debug:

`ocamldebug _build/install/default/bin/emmy`

`(ocd) info modules`

#### To set breakpoint: 

`break @ <module> linenumber`