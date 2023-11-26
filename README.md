### Interp
Tree-walking interpreter implementation of a LISP-type language implemented in OCaml. Toy project. Syntax inspired by Clojure. 


### Build & run:

`opam install --deps-only --with-test .`
`dune exec interp`

#### Tests:
Testing is done with [alcotest](https://github.com/mirage/alcotest).

`dune runtest`

### Debug:

`ocamldebug _build/install/default/bin/interp`

`(ocd) info modules`

#### To set breakpoint: 

`break @ <module> linenumber`